%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin, Nikita Roshchupkin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_chronica).

-export(
   [
    parse_transform/2,
    parse_str_debug/1,
    format_error/1,
    generate_module_iface_name/1
   ]).

-include_lib("pt_lib/include/pt_lib.hrl").
-include("chronica_int.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").
-include_lib("pt_lib/include/pt_error_macro.hrl").

-patrol([{tty, error}]).

parse_transform(AST, Options) ->
    io:setopts([{encoding, unicode}]),
    File = pt_lib:get_file_name(AST),
    ?PATROL_DEBUG("parse transforming: ~s", [File]),
    ?PATROL_DEBUG("options: ~p", [Options]),
    Module = pt_lib:get_module_name(AST),
    AST0 = pt_fun_trace:parse_transform(AST, Options),
    AST1 = AST0,
    Chronica_Tags = find_implicit_tags(AST1, []),
    %% ?PATROL_DEBUG("AST START -------------~n~p~nAST END ---------------", [AST]),

    Iface = generate_module_iface_name(Module),

    {AST2, ListOfId} = pt_lib:replace_fold(AST1, [
        {
            {ast_pattern("log:todo('$String').", Line), Acc},
            begin
                {_, _, Format} = String,
                case pt_lib:is_string(String) of
                    true ->
                        case search_control_symbol(chronica_parser:tokenize_format_string(Format), false) of
                            true ->
                                todo_out(String, control_symbol, File, Module, Line, Acc);
                            false ->
                                todo_out(String, [], File, Module, Line, Acc)
                        end;
                    false ->
                        todo_out(String, dont_const, File, Module, Line, Acc)
                end
            end
        },
        {
            {ast_pattern("log:todo('$String', ...$_...).", Line), Acc},
            begin
                todo_out(String, contain_tags_or_args, File, Module, Line, Acc)
            end
        },
        {
            {ast_pattern("log:$FunName('$String').", Line) = ICall, Acc},
            begin
                fun_arity(FunName, Iface, Module, Line, File,
                 ICall, Acc, {arity_one, String}, Chronica_Tags)
            end
        },
        {
            {ast_pattern("log:$FunName('$String', '$Args').", Line) = ICall, Acc},
            begin
                fun_arity(FunName, Iface, Module, Line, File,
                 ICall, Acc, {arity_two, String, Args}, Chronica_Tags)
            end
        },
        {
            {ast_pattern("log:$FunName('$Tags', '$String', '$Args').", Line) = ICall, Acc},
            begin
                fun_arity(FunName, Iface, Module, Line, File,
                 ICall, Acc, {arity_three, String, Args, Tags}, Chronica_Tags)
            end
        }], []),

    AST3 = add_get_log_tags_fun(lists:usort([[Module] | ListOfId]), AST2),

    AST4 = pt_versioned:parse_transform(AST3, Options),

    AST5 = pt_macro:parse_transform(AST4, Options),

    %% ?PATROL_DEBUG("NEW AST START -------------~n~p~nNEW AST END ---------------~n", [AST5]),
    AST5.

-spec find_implicit_tags(erl_syntax:syntaxTree(), [atom()]) -> [atom()].
find_implicit_tags([], Acc) ->
    lists:usort(Acc);
find_implicit_tags([{attribute, _, chronica_tag, Param} | Tail], Acc) when is_atom(Param)->
    find_implicit_tags(Tail, [Param | Acc]);
find_implicit_tags([{attribute, _, chronica_tag, Param} | Tail], Acc) when is_list(Param)->
    find_implicit_tags(Tail, Param ++ Acc);
find_implicit_tags([_ | Tail], Acc) ->
    find_implicit_tags(Tail, Acc).

search_control_symbol(_, true) ->
    true;
search_control_symbol([], _) ->
    false;
search_control_symbol([Param | Tail], _) ->
    case Param of
        {_, string, _} ->
            search_control_symbol(Tail, false);
        {_, control, _} ->
            search_control_symbol(Tail, true)
    end.

todo_out({_, _, Format}, [], File, Module, Line, Acc) ->
    {ok, Cwd} = file:get_cwd(),
    FullFile = filename:join(Cwd, File),
    io:format("~ts:~p: TODO: " ++ Format ++ "~n", [FullFile, Line]),
    LogId = log_id(Module, Line),
    Tags = [Module, LogId],
    {ast("ok.", Line), [Tags|Acc]};

todo_out(_, View_warrning, File, Module, Line, Acc) ->
    {ok, Cwd} = file:get_cwd(),
    FullFile = filename:join(Cwd, File),
    view_out_warrning(FullFile, Line, View_warrning),
    LogId = log_id(Module, Line),
    Tags = [Module, LogId],
    {ast("ok.", Line), [Tags|Acc]}.

view_out_warrning(FullFile, Line, contain_tags_or_args) ->
        io:format("~ts:~p: Warning: call log:todo() shouldn't contain tags and arguments. ~n", [FullFile, Line]);
view_out_warrning(FullFile, Line, dont_const) ->
        io:format("~ts:~p: Warning: Format in call log:todo() shouldn't be constant. ~n", [FullFile, Line]);
view_out_warrning(FullFile, Line, control_symbol) ->
        io:format("~ts:~p: Warning: Format in call log:todo() shouldn't contain control symbols. ~n", [FullFile, Line]).


wrapParam(Param) ->
    case pt_lib:is_term_or_var(Param) of
        true -> Param;
        false -> ast("fun () -> $Param end.", 0)
    end.

mapFunToPriority({atom, _, debug}) -> {ok, ?P_DEBUG};
mapFunToPriority({atom, _, trace}) -> {ok, ?P_TRACE};
mapFunToPriority({atom, _, info}) -> {ok, ?P_INFO};
mapFunToPriority({atom, _, warning}) -> {ok, ?P_WARNING};
mapFunToPriority({atom, _, error}) -> {ok, ?P_ERROR};
mapFunToPriority(_) -> {error, not_found}.

asttags2list(Tags, Line) ->
    case pt_lib:is_term(Tags) of
        true ->
            _NewTags =
            try
                case erl_syntax:concrete(Tags) of
                    L when is_list(L) -> lists:usort(L);
                    A -> [A]
                end
            catch
                C:E ->
                    ?PATROL_EXCEPTION("Exception: ~p:~p.~nBad format of param ~p", [C, E, Tags]),
                    erlang:error(E)
            end;
        false ->
            throw(?mk_parse_error(Line, non_static_tags))
    end.

fun_arity(Level, Iface, Module, Line, File, ICall, Acc, Arity, Chronica_Tags) ->
    case mapFunToPriority(Level) of
        {ok, Priority} ->
            LogId = log_id(Module, Line),
            Tags = [Module, LogId] ++ Chronica_Tags,
            case Arity of
                {arity_one, String} ->
                    fun_arity_one(Priority, Iface, Tags, Module, Line, File, Acc, String);
                {arity_two, String, Args} ->
                    fun_arity_two(Priority, Iface, Tags, Module, Line, File, Acc, String, Args);
                {arity_three, String, Args, ASTTags} ->
                    NewTags = asttags2list(ASTTags, Line),
                    fun_arity_three(Priority, Iface, Tags ++ NewTags, Module, Line, File, Acc, String, Args)
            end;
        {error, _} ->
            {ICall, Acc}
    end.

fun_arity_one(Priority, Iface, Tags, Module, Line, File, Acc, String) ->
    check_log_params(String, ast("[].", 1), Line),
    NewStringParam = wrapParam(String),
    {ast("chronica_core:log_fast(@Iface, @Priority, @Tags, '@Module', '@Line', '@File', pt_macro_define(function_string), $NewStringParam, []).", Line), [Tags|Acc]}.

fun_arity_two(Priority, Iface, Tags, Module, Line, File, Acc, String, Args) ->
    check_log_params(String, Args, Line),
    NewStringParam = wrapParam(String),
    Positions = detective_stacktrace(Args, [], 0),
    {Args2, Chronica_stacktrace_line} = set_stacktrace_args(Args, Positions, Line),
    NewArgsParam = wrapParam(Args2),
    case Positions of
        [] ->
            {ast("chronica_core:log_fast(@Iface, @Priority, @Tags, @Module, @Line, @File, pt_macro_define(function_string), $NewStringParam, $NewArgsParam).", Line), [Tags|Acc]};
        _ ->
            {ast("begin $Chronica_stacktrace_line = erlang:get_stacktrace(), chronica_core:log_fast(@Iface, @Priority, @Tags, @Module, @Line, @File, pt_macro_define(function_string), $NewStringParam, $NewArgsParam) end.", Line), [Tags|Acc]}
    end.

fun_arity_three(Priority, Iface, NewTags, Module, Line, File, Acc, String, Args) ->
    check_log_params(String, Args, Line),
    NewStringParam = wrapParam(String),
    Positions = detective_stacktrace(Args, [], 0),
    {Args2, Chronica_stacktrace_line} = set_stacktrace_args(Args, Positions, Line),
    NewArgsParam = wrapParam(Args2),
    case Positions of
        [] ->
            {ast("chronica_core:log_fast(@Iface,@Priority, @NewTags, @Module, @Line, @File, pt_macro_define(function_string), $NewStringParam, $NewArgsParam).", Line), [NewTags|Acc]};
        _ ->
            {ast("begin $Chronica_stacktrace_line = erlang:get_stacktrace(), chronica_core:log_fast(@Iface, @Priority, @NewTags, @Module, @Line, @File, pt_macro_define(function_string), $NewStringParam, $NewArgsParam) end.", Line), [NewTags|Acc]}
    end.

detective_stacktrace({var, _, _}, _, _) -> [];
detective_stacktrace({nil, _}, Positions, _) ->
    lists:reverse(Positions);
detective_stacktrace({cons, _, {call, _, {remote, _, {atom, _, erlang}, {atom, _, get_stacktrace}}, _}, Tail}, Positions, CurrentPosition) ->
    detective_stacktrace(Tail, [CurrentPosition | Positions], CurrentPosition + 1);
detective_stacktrace({_, _, _, Tail}, Positions, CurrentPosition) ->
    detective_stacktrace(Tail, Positions, CurrentPosition + 1).

set_stacktrace_args(ArgsParam, [], _Line) ->
    {ArgsParam, ""};
set_stacktrace_args(ArgsParam, Positions, Line) ->
    Chronica_stacktrace_line = erlang:list_to_atom("Chronica_stacktrace" ++ erlang:integer_to_list(Line)),
    {set_activate(ArgsParam, 0, Positions, Chronica_stacktrace_line), {var, Line, Chronica_stacktrace_line}}.

set_activate(ArgsParam, _CurrentPosition, [], _) ->
    ArgsParam;
set_activate({nil, CountLine} = ArgsParam, _, Positions, Chronica_stacktrace_line) ->
    F = fun(_, Acc) ->
            {cons, CountLine, {var, CountLine, Chronica_stacktrace_line}, Acc}
        end,
    lists:foldl(F, ArgsParam, Positions);
set_activate({_, CountLine, _, Tail}, CurrentPosition, [CurrentPosition|T], Chronica_stacktrace_line) ->
    Tail2 = set_activate(Tail, CurrentPosition + 1, T, Chronica_stacktrace_line),
    {cons, CountLine, {var, CountLine, Chronica_stacktrace_line}, Tail2};
set_activate({Type, CountLine, Param, Tail}, CurrentPosition, Positions, Chronica_stacktrace_line) ->
    Tail2 = set_activate(Tail, CurrentPosition + 1, Positions, Chronica_stacktrace_line),
    {Type, CountLine, Param, Tail2}.

parse_str_debug(Str) ->
    ToAST = fun (Str1) ->
        Line = 0,
        case erl_scan:string(Str1, Line) of
            {ok, Tokens, _} ->
                case erl_parse:parse_form(Tokens) of
                    {ok, Abs} -> {ok, [Abs]};
                    {error, ParseFormError} ->
                        case erl_parse:parse_exprs(Tokens) of
                            {ok, List} -> {ok, List};
                            {error, ParseExprsError} ->
                                ?PATROL_ERROR("Error when parsing string  "
                                              "\"~s\"~nparse_form: ~s~n"
                                              "parse_exprs: ~s",
                                [Str1,
                                 pt_supp:format_errors(ParseFormError),
                                 pt_supp:format_errors(ParseExprsError)]),
                                {error, ParseExprsError}
                        end
                end;
            {error, ScanErrorInfo, _ScanEndLocation} ->
                ?PATROL_ERROR("Error when parsing string \"~s\":~n ~s",
                              [Str1, pt_supp:format_errors(ScanErrorInfo)]),
                {error, ScanErrorInfo}
        end
    end,
    {ok, A} = ToAST(Str),
    AST = parse_transform([
                           {attribute, 0, file, {"test.erl", 0}},
                           {attribute, 0, module, mod} | A
                          ], []),
    ResAST = case lists:keytake(module, 3, AST) of
                {value, _, R} -> R;
                false -> false
             end,
    ResStr = pt_lib:ast2str(ResAST),
    io:format("\""++ResStr++"\"", []).

check_log_params({string, Line, Format}, Args, _) when pt_lib:is_list(Args) ->
    case catch pt_lib:list_length(Args) of
        N when is_integer(N) ->
            case args_count(Format, 0, Line) == N of
                true -> ok;
                false ->
                    throw(?mk_parse_error(Line, {bad_log_args_num, pt_lib:ast2str(Args)}))
            end;
        InvalidLength ->
            throw(?mk_parse_error(Line, {invalid_args_length, InvalidLength}))
    end;

check_log_params(VarStr, Args, _Line) when pt_lib:is_variable(VarStr), pt_lib:is_list(Args) ->
    ok;

check_log_params({call, _, _, _}, Args, _Line) when pt_lib:is_list(Args) ->
    ok;

check_log_params({op, _, _, _, _}, Args, _Line) when pt_lib:is_list(Args) ->
    ok;

check_log_params(_Str, {var, Line, Var} = Args, _) ->
    case atom_to_list(Var) of
        "_" ++ _ -> ok;
        _ -> throw(?mk_parse_error(Line, {list_forget_var, pt_lib:ast2str(Args)}))
    end;
check_log_params(Str, Args, Line) ->
    throw(?mk_parse_error(Line, {invalid_args, pt_lib:ast2str(Str), pt_lib:ast2str(Args)})).

args_count([], N, _Line) -> N;
args_count([$~ | Tail], N, Line) ->
    {K, NewTail} = args_count2(Tail, Line),
    args_count(NewTail, N + K, Line);
args_count([_ | Tail], N, Line) ->
    args_count(Tail, N, Line).

args_count2([C | Tail], Line) when C == $.; C == $-;
                             C == $0; C == $1;
                             C == $2; C == $3;
                             C == $4; C == $5;
                             C == $6; C == $7;
                             C == $8; C == $9 ->
    args_count2(Tail, Line);
args_count2([C | Tail], _Line) when C == $~; C == $n ->
    {0, Tail};
args_count2([C | Tail], _Line) when C == $c; C == $f;
                             C == $e; C == $g;
                             C == $s; C == $w;
                             C == $p; C == $B;
                             C == $#; C == $b;
                             C == $+; C == $i;
                             C == $t ->
    {1, Tail};
args_count2([C | Tail], _Line) when C == $W; C == $P;
                             C == $X; C == $x;
                             C == $s; C == $w;
                             C == $p ->
    {2, Tail};
args_count2(Tail, Line) -> throw(?mk_parse_error(Line, {bad_log_param, Tail})).

add_get_log_tags_fun(ListOfProfiles, AST) ->
    pt_lib:add_function(AST, ast("get_log_tags() -> @ListOfProfiles.", 0)).

generate_module_iface_name(Module) ->
    case string:tokens(erlang:atom_to_list(Module), ".") of
        [_] -> generate_module_iface_name_(Module);
        Tokens -> generate_module_iface_name_([erlang:list_to_atom(T) || T <- Tokens])
    end.

generate_module_iface_name_(ModuleName) when is_list(ModuleName) -> % list of atoms, not string
    [Last | Other] = lists:reverse(ModuleName),
    concat_module(lists:reverse([generate_module_iface_name_(Last)|Other]));

generate_module_iface_name_(ModuleName) when is_atom(ModuleName) ->
    erlang:list_to_atom("chronica_iface_" ++ erlang:atom_to_list(ModuleName)).

concat_module([First | Atoms]) when is_list(Atoms) ->
    Name =
        lists:foldl(
            fun (A, Acc) ->
                Acc ++ "." ++ erlang:atom_to_list(A)
            end, erlang:atom_to_list(First), Atoms),
    erlang:list_to_atom(Name).

format_error({list_forget_var, Args}) ->
    io_lib:format("Args parameter should be list: ~p, (use _ to skip error)", [Args]);
format_error({invalid_args, Str, Args}) ->
    NewStr = lists:reverse(lists:foldl(fun ($~, Acc) -> [$~, $~|Acc];
                                           (C, Acc)  -> [C|Acc]
                                       end, "", Str)),
    io_lib:format("Invalid args. Format must be string. Args must be list of terms. Format: ~s, Args: ~s", [NewStr, Args]);
format_error({invalid_args_length, InvalidLength}) ->
    io_lib:format("Impossible error. pt_lib:list_length return ~p.", [InvalidLength]);
format_error({list_forget, Args}) ->
    io_lib:format("Args parameter should be list: ~p", [Args]);
format_error({bad_log_param, Format}) ->
    EscapedFormat = lists:reverse(lists:foldl(fun ($~, Acc) -> [$~, $~|Acc];
                                                  (C, Acc)  -> [C|Acc]
                                              end, "", Format)),
     io_lib:format("Bad log parameter: ~p~n", [EscapedFormat]);
format_error({bad_log_args_num, Param}) ->
    io_lib:format("Wrong args count: ~p~n", [Param]);
format_error(non_static_tags) ->
    "Non static log tags are forbidden";
format_error(Unknown) ->
    io_lib:format("Unknown error: ~p~n", [Unknown]).

log_id(Module, Line) ->
    erlang:list_to_atom(lists:flatten(io_lib:format("~s_~b", [Module, Line]))).

%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin, Nikita Roshchupkin
%%% @copyright (C) 2017, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_chronica_default).

-export([
    todo_out/6,
    fun_arity/9
]).

-include("chronica_int.hrl").
-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").
-include_lib("pt_lib/include/pt_error_macro.hrl").

-patrol([{tty, error}]).

parse_transform(AST, _) ->
    AST.

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

log_id(Module, Line) ->
    erlang:list_to_atom(lists:flatten(io_lib:format("~s_~b", [Module, Line]))).

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

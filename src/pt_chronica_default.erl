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

%%%===================================================================
%%% Internal API
%%%===================================================================
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
                    case asttags2list(ASTTags, Line) of
                        {error, _} = Reason ->
                            Reason;
                        NewTags ->
                            fun_arity_three(
                                Priority, Iface, Tags ++ NewTags, Module,
                                Line, File, Acc, String, Args
                            )
                    end
            end;
        {error, _} ->
            {ICall, Acc}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
detective_stacktrace({var, _, _}, _, _) -> [];
detective_stacktrace({nil, _}, Positions, _) ->
    lists:reverse(Positions);
detective_stacktrace(
        {cons, _, {call, _, {remote, _, {atom, _, erlang}, {atom, _, get_stacktrace}}, _}, Tail},
        Positions,
        CurrentPosition
    ) ->
    detective_stacktrace(Tail, [CurrentPosition | Positions], CurrentPosition + 1);
detective_stacktrace({_, _, _, Tail}, Positions, CurrentPosition) ->
    detective_stacktrace(Tail, Positions, CurrentPosition + 1).

set_stacktrace_args(ArgsParam, [], _Line) ->
    {ArgsParam, ""};
set_stacktrace_args(ArgsParam, Positions, Line) ->
    Chronica_stacktrace_line = erlang:list_to_atom(
        "Chronica_stacktrace" ++ erlang:integer_to_list(Line)
    ),
    SetActivate = set_activate(ArgsParam, 0, Positions, Chronica_stacktrace_line),
    {SetActivate, {var, Line, Chronica_stacktrace_line}}.

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
    try pt_lib:list_length(Args) of
        N when is_integer(N) ->
            case args_count(Format, 0, Line) of
                N -> ok;
                {error, _} = Reason ->
                    Reason;
                _ ->
                    {error, {Line, {bad_log_args_num, pt_lib:ast2str(Args)}}}
            end
    catch
        _:InvalidLength ->
            {error, {Line, {invalid_args_length, InvalidLength}}}
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
        _ -> {error, {Line, {list_forget_var, pt_lib:ast2str(Args)}}}
    end;
check_log_params(Str, Args, Line) ->
    {error, {Line, {invalid_args, pt_lib:ast2str(Str), pt_lib:ast2str(Args)}}}.

args_count([], N, _Line) -> N;
args_count([$~ | Tail], N, Line) ->
    case args_count2(Tail, Line) of
        {error, _} = Reason ->
            Reason;
        {K, NewTail} ->
            args_count(NewTail, N + K, Line)
    end;
args_count([_ | Tail], N, Line) ->
    args_count(Tail, N, Line).

args_count2([C | Tail], Line) when C == $.; C == $-; (C >= $0 andalso C =< $9) ->
    args_count2(Tail, Line);
args_count2([C | Tail], _Line) when C == $~; C == $n ->
    {0, Tail};
args_count2([C | Tail], _Line) when C == $c; C == $f; C == $e; C == $g; C == $s; C == $w;
                                    C == $p; C == $B; C == $#; C == $b; C == $+; C == $i;
                                    C == $t ->
    {1, Tail};
args_count2([C | Tail], _Line) when C == $W; C == $P; C == $X; C == $x; C == $s; C == $w;
                                    C == $p ->
    {2, Tail};
args_count2(Tail, Line) -> {error, {Line, {bad_log_param, Tail}}}.

log_id(Module, Line) ->
    erlang:list_to_atom(lists:flatten(io_lib:format("~s_~b", [Module, Line]))).

asttags2list(Tags, Line) ->
    case pt_lib:is_term(Tags) of
        true ->
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
            {error, {Line, non_static_tags}}
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

fun_arity_one(Priority, Iface, Tags, Module, Line, File, Acc, String) ->
    case check_log_params(String, ast("[].", 1), Line) of
        ok ->
            NewStringParam = wrapParam(String),
            AST = ast(
                "chronica_core:log_fast(
                    @Iface,
                    @Priority,
                    @Tags,
                    '@Module',
                    '@Line',
                    '@File',
                    pt_macro_define(function_string),
                    $NewStringParam,
                    []
                ).", Line
            ),
            {AST, [Tags|Acc]};
        Reason ->
            Reason
    end.

fun_arity_two(Priority, Iface, Tags, Module, Line, File, Acc, String, Args) ->
    case check_log_params(String, Args, Line) of
        ok ->
            NewStringParam = wrapParam(String),
            Positions = detective_stacktrace(Args, [], 0),
            {Args2, Chronica_stacktrace_line} = set_stacktrace_args(Args, Positions, Line),
            NewArgsParam = wrapParam(Args2),
            case Positions of
                [] ->
                    AST = ast(
                        "chronica_core:log_fast(
                            @Iface,
                            @Priority,
                            @Tags,
                            @Module,
                            @Line,
                            @File,
                            pt_macro_define(function_string),
                            $NewStringParam,
                            $NewArgsParam
                        ).", Line
                    ),
                    {AST, [Tags|Acc]};
                _ ->
                    AST = ast(
                        "begin
                            $Chronica_stacktrace_line = erlang:get_stacktrace(),
                            chronica_core:log_fast(
                                @Iface,
                                @Priority,
                                @Tags,
                                @Module,
                                @Line,
                                @File,
                                pt_macro_define(function_string),
                                $NewStringParam,
                                $NewArgsParam
                            )
                        end.", Line
                    ),
                    {AST, [Tags|Acc]}
            end;
        Reason ->
            Reason
    end.

fun_arity_three(Priority, Iface, NewTags, Module, Line, File, Acc, String, Args) ->
    case check_log_params(String, Args, Line) of
        ok ->
            NewStringParam = wrapParam(String),
            Positions = detective_stacktrace(Args, [], 0),
            {Args2, Chronica_stacktrace_line} = set_stacktrace_args(Args, Positions, Line),
            NewArgsParam = wrapParam(Args2),
            case Positions of
                [] ->
                    AST = ast(
                        "chronica_core:log_fast(
                            @Iface,
                            @Priority,
                            @NewTags,
                            @Module,
                            @Line,
                            @File,
                            pt_macro_define(function_string),
                            $NewStringParam,
                            $NewArgsParam
                        ).", Line
                    ),
                    {AST, [NewTags|Acc]};
                _ ->
                    AST = ast("
                        begin
                            $Chronica_stacktrace_line = erlang:get_stacktrace(),
                            chronica_core:log_fast(
                                @Iface,
                                @Priority,
                                @NewTags,
                                @Module,
                                @Line,
                                @File,
                                pt_macro_define(function_string),
                                $NewStringParam,
                                $NewArgsParam
                            )
                        end.", Line
                    ),
                    {AST, [NewTags|Acc]}
            end;
        Reason ->
            Reason
    end.

view_out_warrning(FullFile, Line, contain_tags_or_args) ->
    Format = "~ts:~p: Warning: call log:todo() shouldn't contain tags and arguments. ~n",
    io:format(Format, [FullFile, Line]);
view_out_warrning(FullFile, Line, dont_const) ->
    Format = "~ts:~p: Warning: Format in call log:todo() shouldn't be constant. ~n",
    io:format(Format, [FullFile, Line]);
view_out_warrning(FullFile, Line, control_symbol) ->
    Format = "~ts:~p: Warning: Format in call log:todo() shouldn't contain control symbols. ~n",
    io:format(Format, [FullFile, Line]).

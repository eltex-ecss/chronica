%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Nikita Roshchupkin
%%% @copyright (C) 2017, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_chronica_optimization).

-export([
    init_match_var/2,
    creat_data_log_ast/0,
    format_warning/2
]).

-include("chronica_pt.hrl").
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
%init_match_var
init_match_var([StatVar | TailClause], Acc) ->
    #stat_var{
        active_var = ActiveVar,
        var_log = VarLog,
        deactive_var_log = DeactiveVarLog,
        deactive_var = DeactiveVar,
        clause = Clause,
        type_clause = TypeClause
    } = StatVar,
    FilterVarLog = filter_var(VarLog, maps:new(), any_var),
    case FilterVarLog of
        [] ->
            init_match_var(TailClause, Acc);
        _ ->
            {FuncVar, NewClause} = return_clause_header(TypeClause, Clause),
            NewActiveVar = filter_var(FuncVar, ActiveVar, any_var),
            NewDeactiveVar = filter_var(
                log_replace(clause_replace(NewClause)), DeactiveVar, any_var
            ),
            NewDeactiveVar2 = maps:fold(check_active_var(), NewDeactiveVar, NewActiveVar),
            {NewDeactiveVar3, NewActiveVar2} = maps:fold(
                deactive_into_active(), {NewDeactiveVar2, NewActiveVar}, NewDeactiveVar2
            ),
            case maps:fold(check_active_var(), FilterVarLog, NewActiveVar2) of
                [] ->
                    init_match_var(TailClause, Acc);
                _ ->
                    Log = pt_lib:match(clause_replace(NewClause), ast_pattern("log:$_(...$_...).")),
                    UnfilterDeactiveVarLog = [lists:last(ParamLog) || {_, _, _, ParamLog} <- Log],
                    DeactiveVarLog2 = filter_var(UnfilterDeactiveVarLog, DeactiveVarLog, any_var),
                    DeactiveVarLog3 = maps:fold(check_active_var(), DeactiveVarLog2, NewActiveVar2),
                    NewStatVar = StatVar#stat_var{
                        active_var = NewActiveVar2,
                        deactive_var_log = DeactiveVarLog3,
                        deactive_var = NewDeactiveVar3,
                        clause = NewClause
                    },
                    init_match_var(TailClause, [NewStatVar | Acc])
            end
    end;
init_match_var(_, Acc) ->
    final_match_var(Acc, []).

%creat_data_log_ast
creat_data_log_ast() ->
    fun
        ({type_func, AST}, Acc) ->
            {_, _, NameFunc, _, ClausesAST} = AST,
            DataLogAST = [
                init_stat_var(NameFunc, ClauseAST, VarLogAST, maps:new(), type_func) ||
                    {ClauseAST, VarLogAST} <- lists:foldl(pattern_log(), [], ClausesAST)
            ],
            DataLogAST ++ Acc;
        ({type_case, AST}, Acc) ->
            {_, _, VarAST, ClausesAST} = AST,
            DataLogAST = [
                init_stat_var(undef, ClauseAST, VarLogAST, VarAST, type_case) ||
                    {ClauseAST, VarLogAST} <- lists:foldl(pattern_log(), [], ClausesAST)
            ],
            DataLogAST ++ Acc;
        (_, Acc) ->
            Acc
    end.

%format_warning
format_warning([{Var, {Line, _}} | TailListWarning], FullFile) ->
    io:format(
        "~ts:~p: Warning: variable ~p is unused anywhere except logs of chronica. ~n",
        [FullFile, Line, Var]
    ),
    format_warning(TailListWarning, FullFile);
format_warning(_, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
final_match_var([StatVar | TailStateLog], Acc) ->
    #stat_var{
        deactive_var = DeactiveVar,
        deactive_var_log = DeactiveVarLog,
        active_var = ActiveVar,
        clause = Clause
    } = StatVar,
    NewClause = pt_lib:first_clause(Clause, ast_pattern("(...$_...) -> ...$_... .", _)),
    NewClause2 = match_var(NewClause, []),
    case NewClause2 of
        [] ->
            final_match_var(TailStateLog, [DeactiveVarLog | Acc]);
        _ ->
            Data = lists:foldl(creat_data_log_ast(), [], NewClause2),
            FindDeactiveVar =
                fun(LocStatVar, AccDeactiveVar) ->
                    {_, LocNewClause} = return_clause_header(
                        LocStatVar#stat_var.type_clause, LocStatVar#stat_var.clause
                    ),
                    filter_var(log_replace(LocNewClause), AccDeactiveVar, repeate_var)
                end,
            DeactiveVar2 = lists:foldl(FindDeactiveVar, DeactiveVar, Data),
            {_, ActiveVar2} = maps:fold(
                deactive_into_active(), {DeactiveVar2, ActiveVar}, DeactiveVar2
            ),
            DataStateLog = [
                LocStatVar#stat_var{
                    deactive_var = DeactiveVar,
                    deactive_var_log = DeactiveVarLog,
                    active_var = filter_var(
                        LocStatVar#stat_var.active_var, ActiveVar2, any_var
                    )
                }
                || LocStatVar <- lists:foldl(creat_data_log_ast(), [], NewClause2)
            ],
            ResDeactiveLog = init_match_var(DataStateLog, []),
            final_match_var(TailStateLog, ResDeactiveLog ++ Acc)
    end;
final_match_var(_, Acc) ->
    Acc.

return_clause_header(TypeClause, Clause) ->
    case TypeClause of
        type_func ->
            {_, _, LocFuncVar, _, LocNewClause} = Clause,
            {LocFuncVar, LocNewClause};
        type_case ->
            {_, _, LocFuncVar, _, LocNewClause} = Clause,
            {LocFuncVar, LocNewClause}
    end.

match_var([{type_case, AST} | TailClauseAST], Acc) ->
    [ClauseAST | _] = pt_lib:match(AST, ast_pattern("case $_ of [...$_...] end.", _)),
    match_var(TailClauseAST, [{type_case, ClauseAST} | Acc]);
match_var([{type_fun, AST} | TailClauseAST], Acc) ->
    [ClauseAST | _] = pt_lib:match(AST, ast_pattern("fun [...$_...] end.", _)),
    match_var(TailClauseAST, [{type_fun, ClauseAST} | Acc]);
match_var([{type_if, AST} | TailClauseAST], Acc) ->
    [ClauseAST | _] = pt_lib:match(AST, ast_pattern("if [...$_...] end.", _)),
    match_var(TailClauseAST, [{type_if, ClauseAST} | Acc]);
match_var([{type_receive, AST} | TailClauseAST], Acc) ->
    [ClauseAST | _] = pt_lib:match(AST, ast_pattern("receive [...$_...] end.", _)),
    match_var(TailClauseAST, [{type_receive, ClauseAST} | Acc]);
match_var([{type_receive_after, AST} | TailClauseAST], Acc) ->
    [ClauseAST | _] = pt_lib:match(AST, ast_pattern("receive [...$_...] after $_ -> ...$_... end.", _)),
    match_var(TailClauseAST, [{type_receive_after, ClauseAST} | Acc]);
match_var([{type_try_catch, AST} | TailClauseAST], Acc) ->
    [ClauseAST | _] = pt_lib:match(AST, ast_pattern("try ...$_... catch [...$_...] end.", _)),
    match_var(TailClauseAST, [{type_try_catch, ClauseAST} | Acc]);
match_var([{type_try_case_catch, AST} | TailClauseAST], Acc) ->
    [ClauseAST | _] = pt_lib:match(AST, ast_pattern("try ...$_... of [...$_...] catch [...$_...] end.", _)),
    match_var(TailClauseAST, [{type_try_case_catch, ClauseAST} | Acc]);
match_var([{type_try_after, AST} | TailClauseAST], Acc) ->
    [ClauseAST | _] = pt_lib:match(AST, ast_pattern("try ...$_... after ...$_... end.", _)),
    match_var(TailClauseAST, [{type_try_after, ClauseAST} | Acc]);
match_var([{type_try_case_after, AST} | TailClauseAST], Acc) ->
    [ClauseAST | _] = pt_lib:match(AST, ast_pattern("try ...$_... of [...$_...] after ...$_... end.", _)),
    match_var(TailClauseAST, [{type_try_case_after, ClauseAST} | Acc]);
match_var([{type_try_catch_after, AST} | TailClauseAST], Acc) ->
    [ClauseAST | _] = pt_lib:match(AST, ast_pattern("try ...$_... catch [...$_...] after ...$_... end.", _)),
    match_var(TailClauseAST, [{type_try_catch_after, ClauseAST} | Acc]);
match_var([{type_try_case_catch_after, AST} | TailClauseAST], Acc) ->
    [ClauseAST | _] = pt_lib:match(AST, ast_pattern("try ...$_... of [...$_...] catch [...$_...] after ...$_... end.", _)),
    match_var(TailClauseAST, [{type_try_case_catch_after, ClauseAST} | Acc]);
match_var([{type_undef, _} | TailClauseAST], Acc) ->
    match_var(TailClauseAST, Acc);
match_var([], Acc) ->
    Acc.

filter_var(VarAST, Maps, TypeUpdate) ->
    FilterVar =
        fun(AST, LocMaps) ->
            LocVarAST = pt_lib:match(AST, ast_pattern("$Var.", _), pt_lib:is_variable(Var)),
            case TypeUpdate of
                any_var ->
                    lists:foldl(maps_update_count(), LocMaps, LocVarAST);
                _ ->
                    lists:foldl(maps_update_uuid_count(), LocMaps, LocVarAST)
            end
        end,
    FilterVar(VarAST, Maps).

deactive_into_active() ->
    fun(KeyVarAST, {Line, CountVarAST}, {MapDeactive, MapActive} = Map) ->
        case CountVarAST > 1 of
            true ->
                NewMapDeactive = maps:remove(KeyVarAST, MapDeactive),
                NewMapActive = maps:put(KeyVarAST, {Line, CountVarAST}, MapActive),
                {NewMapDeactive, NewMapActive};
            false ->
                Map
        end
    end.

check_active_var() ->
    fun(KeyVarAST, _, Acc) ->
        maps:remove(KeyVarAST, Acc)
    end.

init_stat_var(NameFunc, ClauseAST, VarLogAST, ActiveVarAST, TypeClause) ->
    #stat_var{
        name_func = NameFunc,
        clause = ClauseAST,
        var_log = VarLogAST,
        active_var = ActiveVarAST,
        type_clause = TypeClause
    }.

pattern_log() ->
    fun(ClauseAST, Acc) ->
        case pt_lib:match(ClauseAST, ast_pattern("log:$_(...$_...).")) of
            [] ->
                Acc;
            LogAST ->
                VarLogAST = [lists:last(ParamLogAST) || {_, _, _, ParamLogAST} <- LogAST],
                [{ClauseAST, VarLogAST} | Acc]
        end
    end.

maps_update_count() ->
    fun({_, Line, Var}, Map) ->
        Fun = fun({L, V}) -> {L, V + 1} end,
        maps:update_with(Var, Fun, {Line, 1}, Map)
    end.

maps_update_uuid_count() ->
    fun({_, _, Var}, Map) ->
        Fun = fun({L, V}) -> {L, V + 1} end,
        case maps:is_key(Var, Map) of
            true ->
                maps:update_with(Var, Fun, Map);
            false ->
                Map
        end
    end.

clause_replace(ClauseAST) ->
    ClauseAST2 = pt_lib:replace(
        ClauseAST, ast_pattern("case $_ of [...$_...] end.", Line), ast("ok.", Line)
    ),
    ClauseAST3 = pt_lib:replace(
        ClauseAST2, ast_pattern("fun [...$_...] end.", Line), ast("ok.", Line)
    ),
    ClauseAST4 = pt_lib:replace(
        ClauseAST3, ast_pattern("receive [...$_...] end.", Line), ast("ok.", Line)
    ),
    ClauseAST5 = pt_lib:replace(
        ClauseAST4,
        ast_pattern("receive [...$_...] after $_ -> ...$_... end.", Line),
        ast("ok.", Line)
    ),
    ClauseAST6 = pt_lib:replace(
        ClauseAST5,
        ast_pattern("try ...$_... catch [...$_...] end.", Line),
        ast("ok.", Line)
    ),
    ClauseAST7 = pt_lib:replace(
        ClauseAST6,
        ast_pattern("try ...$_... of [...$_...] catch [...$_...] end.", Line),
        ast("ok.", Line)
    ),
    ClauseAST8 = pt_lib:replace(
        ClauseAST7,
        ast_pattern("try ...$_... of [...$_...] catch [...$_...] after ...$_... end.", Line),
        ast("ok.", Line)
    ),
    ClauseAST9 = pt_lib:replace(
        ClauseAST8,
        ast_pattern("try ...$_... catch [...$_...] after ...$_... end.", Line),
        ast("ok.", Line)
    ),
    ClauseAST10 = pt_lib:replace(
        ClauseAST9,
        ast_pattern("try ...$_... after ...$_... end.", Line),
        ast("ok.", Line)
    ),
    ClauseAST11 = pt_lib:replace(
        ClauseAST10,
        ast_pattern("try ...$_... of [...$_...] after ...$_... end.", Line),
        ast("ok.", Line)
    ),
    pt_lib:replace(
        ClauseAST11, ast_pattern("if [...$_...] end.", Line), ast("ok.", Line)
    ).

log_replace(LogAST) ->
    pt_lib:replace(LogAST, ast_pattern("log:$_(...$_...).", Line), ast("ok.", Line)).

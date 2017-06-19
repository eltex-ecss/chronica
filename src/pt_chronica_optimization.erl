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
    create_data_log_ast/2,
    format_warning/2,
    delete_ignored_var/2
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
            NewDeactiveVar2 = maps:fold(fun check_active_var/3, NewDeactiveVar, NewActiveVar),
            NewActiveVar2 = maps:fold(
                deactive_into_active(2),
                NewActiveVar,
                lists:foldl(fun find_deactive_var/2, NewDeactiveVar2, [StatVar])
            ),
            NewDeactiveVar3 = maps:fold(fun check_active_var/3, NewDeactiveVar2, NewActiveVar2),
            case maps:fold(fun check_active_var/3, FilterVarLog, NewActiveVar2) of
                [] ->
                    init_match_var(TailClause, Acc);
                _ ->
                    Log = pt_lib:match(clause_replace(NewClause), ast_pattern("log:$_(...$_...).")),
                    UnfilterDeactiveVarLog = [lists:last(ParamLog) || {_, _, _, ParamLog} <- Log],
                    DeactiveVarLog2 = filter_var(UnfilterDeactiveVarLog, DeactiveVarLog, any_var_save_line),
                    DeactiveVarLog3 = maps:fold(fun check_active_var/3, DeactiveVarLog2, NewActiveVar2),
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

create_data_log_ast({type_func, AST}, Acc) ->
    {_, _, NameFunc, _, ClausesAST} = AST,
    DataLogAST = [
        init_stat_var(NameFunc, ClauseAST, VarLogAST, maps:new(), type_func) ||
            {ClauseAST, VarLogAST} <- lists:foldl(fun pattern_log/2, [], ClausesAST)
    ],
    DataLogAST ++ Acc;
create_data_log_ast({type_case, AST}, Acc) ->
    {_, _, VarAST, ClausesAST} = AST,
    DataLogAST = [
        init_stat_var(undef, ClauseAST, VarLogAST, VarAST, type_case) ||
            {ClauseAST, VarLogAST} <- lists:foldl(fun pattern_log/2, [], ClausesAST)
    ],
    DataLogAST ++ Acc;
create_data_log_ast({type_receive, AST}, Acc) ->
    {_, _, ClausesAST} = AST,
    DataLogAST = [
        init_stat_var(undef, ClauseAST, VarLogAST, [], type_receive) ||
            {ClauseAST, VarLogAST} <- lists:foldl(fun pattern_log/2, [], ClausesAST)
    ],
    DataLogAST ++ Acc;
create_data_log_ast({type_receive_after, AST}, Acc) ->
    {_, _, ClausesAST, VarAfterAST, AfterAST} = AST,
    NewClausesAST = [{clause, 0, [VarAfterAST], [], AfterAST} | ClausesAST],
    DataLogAST = [
        init_stat_var(undef, ClauseAST, VarLogAST, [], type_receive_after) ||
            {ClauseAST, VarLogAST} <- lists:foldl(fun pattern_log/2, [], NewClausesAST)
    ],
    DataLogAST ++ Acc;
create_data_log_ast({type_if, AST}, Acc) ->
    {_, _, ClausesAST} = AST,
    DataLogAST = [
        init_stat_var(undef, ClauseAST, VarLogAST, [], type_if) ||
            {ClauseAST, VarLogAST} <- lists:foldl(fun pattern_log/2, [], ClausesAST)
    ],
    DataLogAST ++ Acc;
create_data_log_ast({type_try_catch, AST}, Acc) ->
    {_, _, VarTryAST, [], ClausesAST, []} = AST,
    NewClausesAST = [{clause, 0, [], [], VarTryAST} | ClausesAST],
    DataLogAST = [
        init_stat_var(undef, ClauseAST, VarLogAST, [], type_try_catch) ||
            {ClauseAST, VarLogAST} <- lists:foldl(fun pattern_log/2, [], NewClausesAST)
    ],
    DataLogAST ++ Acc;
create_data_log_ast({type_try_case_catch, AST}, Acc) ->
    {_, _, VarTryAST, ClausesAST1, ClausesAST2, []} = AST,
    NewClausesAST = ClausesAST1 ++ ClausesAST2,
    DataLogAST = [
        init_stat_var(undef, ClauseAST, VarLogAST, [VarTryAST], type_try_case_catch) ||
            {ClauseAST, VarLogAST} <- lists:foldl(fun pattern_log/2, [], NewClausesAST)
    ],
    DataLogAST ++ Acc;
create_data_log_ast({type_try_after, AST}, Acc) ->
    {_, _, VarTryAST, [], [], AfterAST} = AST,
    ClausesAST = [{clause, 0, [], [], AfterAST}, {clause, 0, [], [], VarTryAST}],
    DataLogAST = [
        init_stat_var(undef, ClauseAST, VarLogAST, [], type_try_after) ||
            {ClauseAST, VarLogAST} <- lists:foldl(fun pattern_log/2, [], ClausesAST)
    ],
    DataLogAST ++ Acc;
create_data_log_ast({type_try_case_after, AST}, Acc) ->
    {_, _, VarTryAST, ClausesAST, [], AfterAST} = AST,
    NewClausesAST = [{clause, 0, [], [], AfterAST} | ClausesAST],
    DataLogAST = [
        init_stat_var(undef, ClauseAST, VarLogAST, [VarTryAST], type_try_case_after) ||
            {ClauseAST, VarLogAST} <- lists:foldl(fun pattern_log/2, [], NewClausesAST)
    ],
    DataLogAST ++ Acc;
create_data_log_ast({type_try_catch_after, AST}, Acc) ->
    {_, _, VarTryAST, [], ClausesAST, AfterAST} = AST,
    NewClausesAST = [{clause, 0, [], [], AfterAST}, {clause, 0, [], [], VarTryAST} | ClausesAST],
    DataLogAST = [
        init_stat_var(undef, ClauseAST, VarLogAST, [], type_try_catch_after) ||
            {ClauseAST, VarLogAST} <- lists:foldl(fun pattern_log/2, [], NewClausesAST)
    ],
    DataLogAST ++ Acc;
create_data_log_ast({type_try_case_catch_after, AST}, Acc) ->
    {_, _, VarTryAST, ClausesAST1, ClausesAST2, AfterAST} = AST,
    NewClausesAST1 = ClausesAST1 ++ ClausesAST2,
    NewClausesAST2 = [{clause, 0, [], [], AfterAST} | NewClausesAST1],
    DataLogAST = [
        init_stat_var(undef, ClauseAST, VarLogAST, [VarTryAST], type_try_case_catch_after) ||
            {ClauseAST, VarLogAST} <- lists:foldl(fun pattern_log/2, [], NewClausesAST2)
    ],
    DataLogAST ++ Acc;
create_data_log_ast({type_fun, AST}, Acc) ->
    {_, _, {_, ClausesAST}} = AST,
    DataLogAST = [
        init_stat_var(undef, ClauseAST, VarLogAST, [], type_fun) ||
            {ClauseAST, VarLogAST} <- lists:foldl(fun pattern_log/2, [], ClausesAST)
    ],
    DataLogAST ++ Acc;
create_data_log_ast(_, Acc) ->
    Acc.

format_warning([{Var, Line} | TailListWarning], FullFile) ->
    io:format(
        "~ts:~p: Warning: variable ~p is unused anywhere except logs of chronica ~n",
        [FullFile, Line, Var]
    ),
    format_warning(TailListWarning, FullFile);
format_warning(_, _) ->
    ok.

delete_ignored_var([{Var, _} = DataVar | TailListWarning], Acc) ->
    [IgnoreVarFlag1 | _] = lists:reverse(erlang:atom_to_list(Var)),
    NewAcc =
        case IgnoreVarFlag1 =:= $_ of
            true ->
                Acc;
            false ->
                [DataVar | Acc]
        end,
    delete_ignored_var(TailListWarning, NewAcc);
delete_ignored_var(_, Acc) ->
    lists:reverse(Acc).

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
    NewClause2 =
        case NewClause of
            [] ->
                pt_lib:first_clause(Clause, ast_pattern("if [...$_...] end.", _));
            _ ->
                NewClause
        end,
    NewClause3 = match_var(NewClause2, []),
    case NewClause3 of
        [] ->
            ListDeactiveVarLog = maps:to_list(DeactiveVarLog),
            NewAcc = list_log_to_list(ListDeactiveVarLog, Acc),
            final_match_var(TailStateLog, NewAcc);
        _ ->
            Data = lists:foldl(fun create_data_log_ast/2, [], NewClause3),
            DeactiveVar2 = lists:foldl(fun find_deactive_var/2, DeactiveVar, Data),
            ActiveVar2 = maps:fold(
                deactive_into_active(1), ActiveVar, DeactiveVar2
            ),
            DataStateLog = [
                LocStatVar#stat_var{
                    deactive_var = DeactiveVar,
                    deactive_var_log = DeactiveVarLog,
                    active_var = filter_var(
                        LocStatVar#stat_var.active_var, ActiveVar2, any_var
                    )
                }
                || LocStatVar <- lists:foldl(fun create_data_log_ast/2, [], NewClause3)
            ],
            ResDeactiveLog = init_match_var(DataStateLog, []),
            final_match_var(TailStateLog, ResDeactiveLog ++ Acc)
    end;
final_match_var(_, Acc) ->
    lists:usort(Acc).

find_deactive_var(StatVar, AccDeactiveVar) ->
    {_, NewClause} = return_clause_header(
        StatVar#stat_var.type_clause, StatVar#stat_var.clause
    ),
    filter_var(log_replace(NewClause), AccDeactiveVar, repeate_var).

return_clause_header(type_if, Clause) ->
    {_, _, Guards, FuncVar, NewClause} = Clause,
    {FuncVar ++ Guards, NewClause};
return_clause_header(_, Clause) ->
    {_, _, FuncVar, Guards, NewClause} = Clause,
    {FuncVar ++ Guards, NewClause}.

list_log_to_list([{Var, {ListLine, _}} | TailList], Acc) ->
    F =
        fun(Line, LocAcc) ->
            [{Var, Line} | LocAcc]
        end,
    list_log_to_list(TailList, lists:foldl(F, Acc, ListLine));
list_log_to_list(_, Acc) ->
    Acc.


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
                    lists:foldl(fun maps_update_count/2, LocMaps, LocVarAST);
                any_var_save_line ->
                    lists:foldl(fun maps_update_count_and_line/2, LocMaps, LocVarAST);
                _ ->
                    lists:foldl(fun maps_update_uuid_count/2, LocMaps, LocVarAST)
            end
        end,
    FilterVar(VarAST, Maps).

deactive_into_active(Count) ->
    fun(KeyVarAST, CountVarAST, MapActive) ->
        case CountVarAST > Count of
            true ->
                maps:put(KeyVarAST, CountVarAST, MapActive);
            false ->
                MapActive
        end
    end.

check_active_var(KeyVarAST, _, Acc) ->
    maps:remove(KeyVarAST, Acc).

init_stat_var(NameFunc, ClauseAST, VarLogAST, ActiveVarAST, TypeClause) ->
    #stat_var{
        name_func = NameFunc,
        clause = ClauseAST,
        var_log = VarLogAST,
        active_var = ActiveVarAST,
        type_clause = TypeClause
    }.

pattern_log(ClauseAST, Acc) ->
    case pt_lib:match(ClauseAST, ast_pattern("log:$_(...$_...).")) of
        [] ->
            Acc;
        LogAST ->
            VarLogAST = [lists:last(ParamLogAST) || {_, _, _, ParamLogAST} <- LogAST],
            [{ClauseAST, VarLogAST} | Acc]
    end.

maps_update_count({_, _, Var}, Map) ->
    N = maps:get(Var, Map, 0),
    maps:put(Var, N + 1, Map).

maps_update_count_and_line({_, Line, Var}, Map) ->
    {LocList, N} = maps:get(Var, Map, {[], 0}),
    maps:put(Var, {[Line | LocList], N + 1}, Map).

maps_update_uuid_count({_, _, Var}, Map) ->
    case maps:get(Var, Map, false) of
        false ->
            Map;
        N ->
            maps:update(Var, N + 1, Map)
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

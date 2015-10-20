%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%%     Automatically add debug logs at functions (uses log_server by default)
%%%
%%%     Notice: "Leave traces" could break tail recursion.
%%%     Notice: Target file should be compiled with compile arg +fun_trace
%%%
%%%     Usage example:
%%%
%%%     -compile({parse_transform, pt_build_info}).
%%%
%%%     -compile({fun_trace_enter, exported}).
%%%     -compile({fun_trace_leave, [fun1/1, fun2/3]}).
%%%
%%%     -compile(fun_trace_fun, {Module, Function}). % Overload log function, optional, call log_server by default
%%%     %-compile(fun_trace_fun, Function).          % Call local Function
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_fun_trace).

-export([parse_transform/2, format_error/1, generate_format/4]).

-include_lib("pt_lib/include/pt_error_macro.hrl").
-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").

-patrol([
         {tty, error}
         ]).

-define(arg_prefix, "__PT_FUN_TRACE_ARG_ADD_").

parse_transform(AST, Options) ->
    try
        case lists:member(fun_trace, Options) of
            true  -> ok;
            false -> throw(switched_off)
        end,
        CompileOptions = pt_lib:get_compile_options(AST),
        TotalList = get_list_of_fun(AST, CompileOptions, fun_trace),
        EnterList = get_list_of_fun(AST, CompileOptions, fun_trace_enter),
        LeaveList = get_list_of_fun(AST, CompileOptions, fun_trace_leave),
        ModuleName = pt_lib:get_module_name(AST),
        LogTags = generate_log_tags(ModuleName),
        InsertEnterLog = fun
                        (ast_pattern("(...$Args...) when ...$Guards... -> ...$Exprs... .", L) = Source, {FunName, Arity}) ->
                            case lists:keyfind(fun_trace_fun, 1, CompileOptions) of
                                false ->
                                    {NewArgs, {StrEnter, ArgsEnter}, _} = get_log_params(FunName, Args, L),
                                    ast("(...$NewArgs...) when ...$Guards... ->
                                            log:debug(@LogTags, @StrEnter, $ArgsEnter),
                                            ...$Exprs... .", L);
                                {_, {ModuleName, FunName}} when Arity == 5 -> Source;
                                {_, {UModule, UFName}} ->
                                    {NewArgs, AstArgs} = generate_args_list(Args, L),
                                    ast("(...$NewArgs...) when ...$Guards... ->
                                            @UModule:@UFName(enter, {@FunName, @Arity}, $AstArgs, @ModuleName, @L),
                                            ...$Exprs... .", L);
                                {_, FunName} when Arity == 5 -> Source;
                                {_, UFName} when is_atom(UFName) ->
                                    {NewArgs, AstArgs} = generate_args_list(Args, L),
                                    ast("(...$NewArgs...) when ...$Guards... ->
                                            @UFName(enter, {@FunName, @Arity}, $AstArgs, @ModuleName, @L),
                                            ...$Exprs... .", L)
                            end
                     end,
        InsertLeaveLog = fun
                        (ast_pattern("(...$Args...) when ...$Guards... -> ...$Exprs... .", L) = Source, {FunName, Arity}) ->
                            case lists:keyfind(fun_trace_fun, 1, CompileOptions) of
                                false ->
                                    {NewArgs, _, {StrLeave, ArgsLeave}} = get_log_params(FunName, Args, L),
                                    ast("(...$NewArgs...) when ...$Guards... ->
                                            __PT_ADD_TRACES_VAR =
                                                begin
                                                    ...$Exprs...
                                                end,
                                            log:debug(@LogTags, @StrLeave, $ArgsLeave),
                                            __PT_ADD_TRACES_VAR.", L);
                                {_, {ModuleName, FunName}} when Arity == 5 -> Source;
                                {_, {UModule, UFName}} ->
                                    {NewArgs, AstArgs} = generate_args_list(Args, L),
                                    ast("(...$NewArgs...) when ...$Guards... ->
                                            __PT_ADD_TRACES_VAR =
                                                begin
                                                    ...$Exprs...
                                                end,
                                            @UModule:@UFName({leave, __PT_ADD_TRACES_VAR}, {@FunName, @Arity}, $AstArgs, @ModuleName, @L),
                                            __PT_ADD_TRACES_VAR.", L);
                                {_, FunName} when Arity == 5 -> Source;
                                {_, UFName} when is_atom(UFName) ->
                                    {NewArgs, AstArgs} = generate_args_list(Args, L),
                                    ast("(...$NewArgs...) when ...$Guards... ->
                                            __PT_ADD_TRACES_VAR =
                                                begin
                                                    ...$Exprs...
                                                end,
                                            @UFName({leave, __PT_ADD_TRACES_VAR}, {@FunName, @Arity}, $AstArgs, @ModuleName, @L),
                                            __PT_ADD_TRACES_VAR.", L)
                            end
                     end,
        AST1 = add_traces_to_functions(AST,  lists:usort(LeaveList ++ TotalList), InsertLeaveLog),
        AST2 = add_traces_to_functions(AST1, lists:usort(EnterList ++ TotalList), InsertEnterLog),
        AST2
    catch
        throw:switched_off -> AST
    end.

generate_log_tags(ModuleName) when is_atom(ModuleName) ->
    [erlang:list_to_atom(lists:flatten(io_lib:format("fun_trace_~s", [ModuleName])))].

generate_args_list(Args, Line) ->
    {NewArgs, {_, ArgsList}} =
        lists:mapfoldl(
            fun (Arg, {N, Acc}) ->
                NewArg = generate_arg(Arg, N, Line),
                VarName = generate_format_arg(Arg, N, Line),
                {NewArg, {N + 1, ast("[$VarName | $Acc].", Line)}}
            end,
            {0, ast("[].", Line)},
            Args),
    {NewArgs, pt_lib:list_reverse(ArgsList)}.

get_list_of_fun(AST, CompileOptions, Opt) ->
    lists:usort(lists:foldl(
        fun (S, Acc) ->
                case S of
                    {Opt, all} -> add_all(AST, Acc);
                    {Opt, exported} -> add_exported(AST, Acc);
                    {Opt, ListOfFun} when is_list(ListOfFun) -> ListOfFun ++ Acc;
                    {Opt, BadParam} -> throw(?mk_parse_error(0, {bad_param, BadParam}));
                    _ -> Acc
                end
        end,
        [],
        CompileOptions
    )).

add_all(AST, List) ->
    lists:foldl(
        fun ({function, _, Name, Arity, _}, Acc) -> [{Name, Arity} | Acc];
            (_, Acc) -> Acc
        end,
        List,
        AST).

add_exported(AST, List) ->
    lists:foldl(
        fun ({attribute, _, export, L}, Acc) -> L ++ Acc;
            (_, Acc) -> Acc
        end,
        List,
        AST).

add_traces_to_functions(AST, [], _) ->
    AST;
add_traces_to_functions(AST, [{FunName, Arity} | Tail], Modifier) ->
    Fun = case pt_lib:match(AST, ast_pattern("$FunName/$Arity [...$_...].")) of
            [F | _] -> F;
            [] -> throw(?mk_parse_error(0, {fun_not_found, {FunName, Arity}}))
          end,
    ast_pattern("$_/$_ [...$Clauses...].", Line) = Fun,
    NewClauses = lists:map(fun (C) -> Modifier(C, {FunName, Arity}) end, Clauses),
    Res = pt_supp:replace_first(AST,
        fun (X) ->
            case X of
                ast_pattern("$FunName/$Arity [...$_...].", Line) -> ast("$FunName/$Arity [...$NewClauses...].", Line);
                _ -> X
            end
        end),
    add_traces_to_functions(Res, Tail, Modifier).

get_log_params(FunName, Args, Line) ->
    {NewArgs, {_, LogArgsEnter, ArgsStr}} = lists:mapfoldl(
                            fun (Arg, {N, Acc, AStr}) ->
                                {NewArg1, NewAStr, NewAcc} = generate_format({AStr, Acc}, Arg, N, Line),
                                NewArg = generate_arg(NewArg1, N, Line),
                                {NewArg, {N+1, NewAcc, NewAStr}}
                            end, {0, ast("[].", Line), ""}, Args),
    LogArgsEnter2 = LogArgsEnter,
    LogArgsLeave = pt_lib:list_concat(LogArgsEnter2, ast("[__PT_ADD_TRACES_VAR].", Line)),
    LogStrEnter = lists:flatten(io_lib:format("> ~s(~s)", [FunName, ArgsStr])),
    LogStrLeave = lists:flatten(io_lib:format("< ~s(~s), Result: ~~10000000p", [FunName, ArgsStr])),

    {NewArgs, {LogStrEnter, LogArgsEnter2}, {LogStrLeave, LogArgsLeave}}.

should_output_whole_param(ast_pattern("$A = $B.")) ->
    should_output_whole_param(A) and should_output_whole_param(B);
should_output_whole_param(ast_pattern("$A.")) when pt_lib:is_variable(A) ->
    should_output(A);
should_output_whole_param(ast_pattern("#$_{}.")) ->
    true;
should_output_whole_param(ast_pattern("#$_{...$_...}.")) ->
    false;
should_output_whole_param(ast_pattern("{...$_...}.")) ->
    false;
should_output_whole_param(ast_pattern("$A.")) when pt_lib:is_list(A) ->
    false;
should_output_whole_param(_) ->
    true.

process_fun_trace_arg({OldFormat, OldFormatArg}, Arg, N, Line) ->
    case should_output_whole_param(Arg) of
        true ->
            V = generate_format_arg(Arg, N, Line),
            {OldFormat ++ "~10000000p", pt_lib:list_concat(OldFormatArg, ast("[$V].", Line))};
        false ->
            F =
                fun
                    ({OldFormat_, OldFormatArg_}, ast_pattern("$A = $B."), N_, Line_, Y) ->
                        {Format_, FormatArg_} = Y({OldFormat_, OldFormatArg_}, A, N_, Line_, Y),
                        Y({Format_ ++ " = ", FormatArg_}, B, N_, Line_, Y);

                    ({OldFormat_, OldFormatArg_}, ast_pattern("$A."), _N_, _Line_, _Y) when pt_lib:is_variable(A) ->
                        {OldFormat_ ++ get_real_var_name(A), OldFormatArg_};

                    ({OldFormat_, OldFormatArg_}, ast_pattern("$A."), N_, Line_, _Y) ->
                        process_fun_trace_arg2({OldFormat_, OldFormatArg_}, A, N_, Line_)
                end,
            F({OldFormat, OldFormatArg}, Arg, N, Line, F)
    end.

process_fun_trace_arg2({OldFormat, OldFormatArg}, ast_pattern("$A = $B."), N, Line) ->
    {Format, FormatArg} = process_fun_trace_arg2({OldFormat, OldFormatArg}, A, N, Line),
    process_fun_trace_arg2({Format ++ " = ", FormatArg}, B, N, Line);
process_fun_trace_arg2({OldFormat, OldFormatArg}, ast_pattern("[$A | $B]."), N, Line) when pt_lib:is_variable(B) ->
    {Format,  FormatArg}  = process_fun_trace_arg2({OldFormat ++ "[", OldFormatArg}, A, N, Line),
    {Format2, FormatArg2} = process_fun_trace_arg2({Format ++ " | ", FormatArg}, B, N, Line),
    {Format2 ++ "]", FormatArg2};
process_fun_trace_arg2({OldFormat, OldFormatArg}, ast_pattern("[$_A | $_B].") = P, N, Line) ->
    {Format, FormatArg} = process_list_element({OldFormat ++ "[", OldFormatArg}, P, N, Line, true),
    {Format ++ "]", FormatArg};
process_fun_trace_arg2({OldFormat, OldFormatArg}, ast_pattern("#$RecordName{...$Fields...}."), N, Line) ->
    {Format, FormatArg} = process_tuple_elements({OldFormat ++ "#" ++ atom_to_list(RecordName) ++ "{", OldFormatArg}, Fields, N, Line, true),
    {Format ++ "}", FormatArg};
process_fun_trace_arg2({OldFormat, OldFormatArg}, {record_field, _, Name, Value}, N, Line) ->
    process_fun_trace_arg2({OldFormat ++ pt_lib:ast2str(Name) ++ " = ", OldFormatArg}, Value, N, Line);
process_fun_trace_arg2({OldFormat, OldFormatArg}, ast_pattern("$A."), N, Line) when pt_lib:is_tuple(A) ->
    {tuple, _L, List} = A,
    {Format, FormatArg} = process_tuple_elements({OldFormat ++ "{", OldFormatArg}, List, N, Line, true),
    {Format ++ "}", FormatArg};
process_fun_trace_arg2({OldFormat, OldFormatArg}, ast_pattern("_."), _N, _Line) ->
    {OldFormat ++ "_", OldFormatArg};
process_fun_trace_arg2({OldFormat, OldFormatArg}, ast_pattern("State."), _N, _Line) ->
    {OldFormat ++ "State", OldFormatArg};
process_fun_trace_arg2({OldFormat, OldFormatArg}, ast_pattern("$Var."), _N, Line) when pt_lib:is_variable(Var) ->
    case should_output(Var) of
        false ->
            VarName = get_real_var_name(Var),
            {OldFormat ++ VarName, OldFormatArg};
        true ->
            {OldFormat ++ "~10000000p", pt_lib:list_concat(OldFormatArg, ast("[$Var].", Line))}
    end;
process_fun_trace_arg2({OldFormat, OldFormatArg}, P, _N, _Line)  ->
    {OldFormat ++ pt_lib:ast2str(P), OldFormatArg}.

process_tuple_elements({OldFormat, OldFormatArg}, [], _N, _Line, _First) ->
    {OldFormat, OldFormatArg};
process_tuple_elements({OldFormat, OldFormatArg}, [A | B], N, Line, First) ->
    OldFormat2 =
        case First of
            true -> OldFormat;
            false -> OldFormat ++ ", "
        end,
    {Format,  FormatArg}  = process_fun_trace_arg2({OldFormat2, OldFormatArg}, A, N, Line),
    process_tuple_elements({Format, FormatArg}, B, N, Line, false).

process_list_element({OldFormat, OldFormatArg}, ast_pattern("[]."), _N, _Line, _First) ->
    {OldFormat, OldFormatArg};
process_list_element({OldFormat, OldFormatArg}, ast_pattern("[$A | $B]."), N, Line, First) ->
    OldFormat2 =
        case First of
            true -> OldFormat;
            false -> OldFormat ++ ", "
        end,
    {Format,  FormatArg} = process_fun_trace_arg2({OldFormat2, OldFormatArg}, A, N, Line),
    process_list_element({Format, FormatArg}, B, N, Line, false);
process_list_element({OldFormat, OldFormatArg}, ast_pattern("$B."), N, Line, First) ->
    OldFormat2 =
        case First of
            true -> OldFormat;
            false -> OldFormat ++ " | "
        end,
    process_fun_trace_arg2({OldFormat2, OldFormatArg}, B, N, Line).

generate_format({OldFormat, OldFormatArg}, Arg, N, Line) ->
    OldFormat2 = case OldFormat of
                    [] -> [];
                    _ -> OldFormat ++ ", "
                 end,
    VarArg = generate_format_arg(Arg, N, Line),
    NewArg = case Arg of
                ast_pattern("$VarArg = ($AA = $BB).", Line) ->
                    case generate_pt_arg_copy(AA, N) of
                        BB -> AA;
                        _ -> ast("$AA = $BB.", Line)
                    end;
                ast_pattern("$VarArg = $AA.") ->
                    AA;
                _ ->
                    Arg
             end,
    NewArg2 = generate_pt_arg_copy(NewArg, N),
    {F, A} = process_fun_trace_arg({OldFormat2, OldFormatArg}, NewArg2, N, Line),
    {ast("$NewArg = $NewArg2.", Line), F, A}.

generate_pt_arg_copy(ArgAst, N) ->
    pt_lib:replace(ArgAst,
        [
            {ast_pattern("$A."), pt_lib:is_variable(A), generate_pt_arg_var(A, N)},
            {ast_pattern("<<...$_...>>.", Line), ast("_.", Line)}
        ]).

generate_pt_arg_var({var, _Line, '_'} = V, _N) -> V;
generate_pt_arg_var({var, Line, Name}, N) ->
    {var, Line, erlang:list_to_atom(?arg_prefix ++ lists:flatten(io_lib:format("~3.10.0B_", [N])) ++ erlang:atom_to_list(Name))}.

get_real_var_name({var, _, _} = Var) ->
    case pt_lib:ast2str(Var) of
        ?arg_prefix ++ [_, _, _, _ | RealName] -> RealName;
        RealName -> RealName
    end.

generate_format_arg(_Arg, N, Line) ->
    {var, Line, pt_supp:mk_atom("__PT_ADD_TRACES_ARG", N)}.

generate_arg(Arg, N, Line) ->
    VarArg = generate_format_arg(Arg, N, Line),
    case Arg of
        ast_pattern("$VarArg = $_.") ->
            Arg;
        _ ->
            ast("$VarArg = $Arg.", Line)
    end.

should_output(ast_pattern("$A = $B.")) ->
    should_output(A) or should_output(B);
should_output(ast_pattern("State.")) ->
    false;
should_output(ast_pattern("_State.")) ->
    false;
should_output({var, _, _} = Var) ->
    case pt_lib:ast2str(Var) of
        ?arg_prefix ++ [_, _, _, _ | VarName] ->
            case VarName of
                "State" -> false;
                "_" ++ _ -> false;
                _ -> true
            end;
        "_" ++ _ -> false;
        _ -> true
    end;
should_output(_)->
    true.

format_error({bad_param, P}) ->
    io_lib:format("Bad fun_trace param: ~p", [P]);
format_error({fun_not_found, {Name, Arity}}) ->
    io_lib:format("Cant add fun trace to fun ~p/~p, cause not found", [Name, Arity]);
format_error(E) ->
    io_lib:format("Unknown error: ~p", [E]).

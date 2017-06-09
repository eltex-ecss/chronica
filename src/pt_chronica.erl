%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin, Nikita Roshchupkin
%%% @copyright (C) 2017, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pt_chronica).

-export([
    parse_transform/2,
    parse_str_debug/1,
    format_error/1,
    generate_module_iface_name/1
]).

-include("chronica_int.hrl").
-include("chronica_pt.hrl").
-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_lib/include/pt_patrol.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").
-include_lib("pt_lib/include/pt_error_macro.hrl").

-patrol([{tty, error}]).

parse_transform(AST, Options) ->
    check_transform(AST),
    io:setopts([{encoding, unicode}]),
    ?PATROL_DEBUG("options: ~p", [Options]),
    AST0 = pt_fun_trace:parse_transform(AST, Options),
    AST1 = AST0,
    {AST2, ListOfId} = replace_fake_log(AST1, replacement_mode(Options)),
    Module = pt_lib:get_module_name(AST),
    AST3 = add_get_log_tags_fun(lists:usort([[Module] | ListOfId]), AST2),
    AST4 = pt_versioned:parse_transform(AST3, Options),
    AST5 = pt_macro:parse_transform(AST4, Options),
    add_successful_transform(AST5).

check_transform([_HeadAST1, _HeadAST2, {attribute, 0, option, successful_transform} | _AST]) ->
    throw(?mk_parse_error(0, multiple_transform));
check_transform(_AST) ->
    ok.

replacement_mode(CompileOptions) ->
    FlagChronicaDisabled = lists:member(chronica_disabled, CompileOptions),
    case os:getenv("CHRONICA_DISABLED") =/= false orelse FlagChronicaDisabled of
        true ->
            disable_log_mode;
        false ->
            FlagChronicaOptimization = lists:member(chronica_optimization, CompileOptions),
            case os:getenv("CHRONICA_OPTIMIZATION") =/= false orelse FlagChronicaOptimization of
                true ->
                    optimization_log_mode;
                false ->
                    default_log_mode
            end
    end.

replace_fake_log(AST, default_log_mode) ->
    File = pt_lib:get_file_name(AST),
    ?PATROL_DEBUG("parse transforming: ~s", [File]),
    Module = pt_lib:get_module_name(AST),
    Iface = generate_module_iface_name(Module),
    Chronica_Tags = find_implicit_tags(AST, []),
    pt_lib:replace_fold(AST, [
        {
            {ast_pattern("log:todo('$String').", Line), Acc},
            begin
                {_, _, Format} = String,
                case pt_lib:is_string(String) of
                    true ->
                        case search_control_symbol(chronica_parser:tokenize_format_string(Format), false) of
                            true ->
                                pt_chronica_default:todo_out(
                                    String, control_symbol, File, Module, Line, Acc
                                );
                            false ->
                                pt_chronica_default:todo_out(String, [], File, Module, Line, Acc)
                        end;
                    false ->
                        pt_chronica_default:todo_out(String, dont_const, File, Module, Line, Acc)
                end
            end
        },
        {
            {ast_pattern("log:todo('$String', ...$_...).", Line), Acc},
            pt_chronica_default:todo_out(String, contain_tags_or_args, File, Module, Line, Acc)
        },
        {
            {ast_pattern("log:$FunName('$String').", Line) = ICall, Acc},
            pt_chronica_default:fun_arity(
                FunName, Iface, Module, Line, File,
                ICall, Acc, {arity_one, String}, Chronica_Tags
            )
        },
        {
            {ast_pattern("log:$FunName('$String', '$Args').", Line) = ICall, Acc},
            pt_chronica_default:fun_arity(
                FunName, Iface, Module, Line, File,
                ICall, Acc, {arity_two, String, Args}, Chronica_Tags
            )
        },
        {
            {ast_pattern("log:$FunName('$Tags', '$String', '$Args').", Line) = ICall, Acc},
            pt_chronica_default:fun_arity(
                FunName, Iface, Module, Line, File,
                ICall, Acc, {arity_three, String, Args, Tags}, Chronica_Tags
            )
        }], []
    );
replace_fake_log(AST, disable_log_mode) ->
    {AST2, _} = pt_lib:replace_fold(AST, [
        {
            {ast_pattern("log:$_(...$_...).", _Line), Acc},
            {ast("ok.", _Line), Acc}
        }], []
    ),
    {AST2, []};
replace_fake_log(AST, optimization_log_mode) ->
    ListFuncAST = [
        {type_func, LocAST} || LocAST <- pt_lib:match(AST, ast_pattern("$_/$_ [...$_...]."))
    ],
    CreateDataLogAST = pt_chronica_optimization:creat_data_log_ast(),
    DataStateLog = [
        StatVar#stat_var{deactive_var = maps:new(), deactive_var_log = maps:new()} ||
            StatVar <- lists:foldl(CreateDataLogAST, [], ListFuncAST)
    ],
    MatchVar =
        fun(StatVar, Acc) ->
            ListDeactiveLog = pt_chronica_optimization:init_match_var([StatVar], []),
            Flatten =
                fun(DeactiveLog, LocAcc) ->
                    maps:to_list(DeactiveLog) ++ LocAcc
                end,
            lists:foldl(Flatten, [], ListDeactiveLog) ++ Acc
        end,
    ListWarning = lists:foldl(MatchVar, [], DataStateLog),
    File = pt_lib:get_file_name(AST),
    {ok, Cwd} = file:get_cwd(),
    FullFile = filename:join(Cwd, File),
    pt_chronica_optimization:format_warning(ListWarning, FullFile),
    replace_fake_log(AST, default_log_mode).

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

-spec find_implicit_tags(erl_syntax:syntaxTree(), [atom()]) -> [atom()].
find_implicit_tags([], Acc) ->
    lists:usort(Acc);
find_implicit_tags([{attribute, _, chronica_tag, Param} | Tail], Acc) when is_atom(Param)->
    find_implicit_tags(Tail, [Param | Acc]);
find_implicit_tags([{attribute, _, chronica_tag, Param} | Tail], Acc) when is_list(Param)->
    find_implicit_tags(Tail, Param ++ Acc);
find_implicit_tags([_ | Tail], Acc) ->
    find_implicit_tags(Tail, Acc).

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
    AST = parse_transform(
        [
            {attribute, 0, file, {"test.erl", 0}},
            {attribute, 0, module, mod} | A
        ], []
    ),
    ResAST =
        case lists:keytake(module, 3, AST) of
            {value, _, R} -> R;
            false -> false
        end,
    ResStr = pt_lib:ast2str(ResAST),
    io:format("\""++ResStr++"\"", []).

add_successful_transform([HeadAST1, HeadAST2 | AST]) ->
    [HeadAST1, HeadAST2, {attribute, 0, option, successful_transform} | AST].

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
    Name = lists:foldl(
        fun (A, Acc) ->
            Acc ++ "." ++ erlang:atom_to_list(A)
        end, erlang:atom_to_list(First), Atoms),
    erlang:list_to_atom(Name).

format_error({list_forget_var, Args}) ->
    io_lib:format("Args parameter should be list: ~p, (use _ to skip error)", [Args]);
format_error({invalid_args, Str, Args}) ->
    NewStr = lists:reverse(lists:foldl(
        fun
            ($~, Acc) -> [$~, $~|Acc];
            (C, Acc)  -> [C|Acc]
        end, "", Str
    )),
    io_lib:format("Invalid args. Format must be string. Args must be list of terms. Format: ~s, Args: ~s", [NewStr, Args]);
format_error({invalid_args_length, InvalidLength}) ->
    io_lib:format("Impossible error. pt_lib:list_length return ~p.", [InvalidLength]);
format_error({list_forget, Args}) ->
    io_lib:format("Args parameter should be list: ~p", [Args]);
format_error({bad_log_param, Format}) ->
    EscapedFormat = lists:reverse(lists:foldl(
        fun
            ($~, Acc) -> [$~, $~|Acc];
            (C, Acc)  -> [C|Acc]
        end, "", Format
    )),
    io_lib:format("Bad log parameter: ~p~n", [EscapedFormat]);
format_error({bad_log_args_num, Param}) ->
    io_lib:format("Wrong args count: ~p~n", [Param]);
format_error(non_static_tags) ->
    "Non static log tags are forbidden";
format_error(multiple_transform) ->
    "Multiple parse transform";
format_error(Unknown) ->
    io_lib:format("Unknown error: ~p~n", [Unknown]).

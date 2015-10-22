%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin, Nikita Roshchupkin, Constantine Malikov
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_format_creator).

-export([
         reload_formats/2,
         create_format_function/3,
         frame/1,
         insert_tab/1
        ]).

-include("chronica_int.hrl").
-include("chronica_config.hrl").
-include_lib("pt_lib/include/pt_lib.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").

% "%Id %Y-%M(%Ml)-%D %H:%Mi:%S:%Ms %PRIORITY %P %Priority %Pid %File %Line %Module %Function %Message"

reload_formats(FList, Module) ->
    pt_recompilable:recompile_orig_module(
        Module,
        fun (AST, _) ->
            lists:foldl(
                fun
                    (ast_pattern("default/1 [...$_...].") = F, Tree) ->
                        pt_lib:replace(Tree, ast_pattern("default/1 [...$_...]."), F);
                    (ast_pattern("binary/1 [...$_...].") = F, Tree) ->
                        pt_lib:replace(Tree, ast_pattern("binary/1 [...$_...]."), F);
                    (F, Tree) ->
                        pt_lib:add_function(Tree, F)
                end,
                AST,
                FList)
        end).

create_format_function(
                       FunName, UserFormat, #chronica_coloring{colored = Colored,
                                                               formats_name = NameFormats,
                                                               colors_spec = ColorsSpec}
                      ) ->
    ?INT_DBG("create_format_function ~p ~s", [FunName, UserFormat]),

    {ListFUM_rev, _Mode} = run(UserFormat, [{[], []}], undef, fun (Other) -> Other end),
    ListFUM = lists:reverse(ListFUM_rev),
            ?INT_DBG("~p~n~n", [ListFUM]),
            FunArgs =
                fun({_, Args_c}, Acc) ->
                    Acc ++ Args_c
                end,
    Args = lists:foldl(FunArgs, [], ListFUM),
    case Colored of
        true ->
            case lists:keyfind(FunName, 2, NameFormats) of
                {_FormatId, FunName} ->
                    ColorsData = update_colors_data(ColorsSpec, #chronica_colors_data{}),
                    FunColor = fun ({Data, Str}) -> coloring_str(Str, get_color_colors_data(ColorsData, Data)) end,
                    {BinaryStr, FormattingStr} = make_format_function_ast(Args, "<<", "~p({{{Year,Month,Day},{Hour,Minute,Second},Millisecond}, PriorityInt, Module, Pid, Line, File, Function, UserStr}) -> " ++ priority_color(FunColor), FunColor, true),
                    true;
                false ->
                    {BinaryStr, FormattingStr} = make_format_function_ast(Args, "<<", "~p({{{Year,Month,Day},{Hour,Minute,Second},Millisecond}, PriorityInt, Module, Pid, Line, File, Function, UserStr}) -> ", fun ({_Data, Str}) -> Str end, false)
            end;
        false ->
            {BinaryStr, FormattingStr} = make_format_function_ast(Args, "<<", "~p({{{Year,Month,Day},{Hour,Minute,Second},Millisecond}, PriorityInt, Module, Pid, Line, File, Function, UserStr}) -> ", fun ({_Data, Str}) -> Str end, false)
    end,
    ?INT_DBG("BinaryStr: ~p~n~n", [lists:flatten(BinaryStr)]),
    FunctionStr = FormattingStr ++ BinaryStr,
    [AstStr] = pt_lib:str2ast(io_lib:format(FunctionStr, [FunName]), 0),
    AstStr.

priority_color(FunColor) ->
    "Get_priority_short_prefix = fun(Priority) ->
        case Priority of
            1 -> <<" ++ FunColor({'Error', "\"*** ERROR\""}) ++ ">>;
            2 -> <<" ++ FunColor({'Warning', "\"W\""}) ++ ">>;
            3 -> <<" ++ FunColor({'Info', "\"I\""}) ++ ">>;
            4 -> <<" ++ FunColor({'Trace', "\"T\""}) ++ ">>;
            5 -> <<" ++ FunColor({'Debug', "\"D\""}) ++ ">> end end,

    Get_priority_prefix_up = fun(Priority) ->
        case Priority of
            1 -> <<" ++ FunColor({'Error', "\"ERROR\""}) ++ ">>;
            2 -> <<" ++ FunColor({'Warning', "\"WARN \""}) ++ ">>;
            3 -> <<" ++ FunColor({'Info', "\"INFO \""}) ++ ">>;
            4 -> <<" ++ FunColor({'Trace', "\"TRACE\""}) ++ ">>;
            5 -> <<" ++ FunColor({'Debug', "\"DEBUG\""}) ++ ">> end end,

    Get_priority_prefix_low = fun(Priority) ->
        case Priority of
            1 -> <<" ++ FunColor({'Error', "\"error\""}) ++ ">>;
            2 -> <<" ++ FunColor({'Warning', "\"warning\""}) ++ ">>;
            3 -> <<" ++ FunColor({'Info', "\"info\""}) ++ ">>;
            4 -> <<" ++ FunColor({'Trace', "\"trace\""}) ++ ">>;
            5 -> <<" ++ FunColor({'Debug', "\"debug\""}) ++ ">> end end, ".

make_format_function_ast([], BinaryStr, StrAST, _FunColor, _FlagColor) ->
    {BinaryStr ++ ">>.", StrAST};
make_format_function_ast([Args | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    case Args of
        'Year' ->
            NewBinaryStr = FunColor({Args, "Y_binary/binary"}),
            NewStrAST = "Y_binary = year_binary(Year), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'Month' ->
            NewBinaryStr = FunColor({Args, "Mo_binary/binary"}),
            NewStrAST = "Mo_binary = time(Month), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'Month_string' ->
            NewBinaryStr = FunColor({Args, "String_Mo_binary/binary"}),
            NewStrAST = "String_Mo_binary = month_str_binary(Month), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'Day' ->
            NewBinaryStr = FunColor({Args, "D_binary/binary"}),
            NewStrAST = "D_binary = time(Day), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'Hour' ->
            NewBinaryStr = FunColor({Args, "H_binary/binary"}),
            NewStrAST = "H_binary = time(Hour), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'Minute' ->
            NewBinaryStr = FunColor({Args, "Minute_binary/binary"}),
            NewStrAST = "Minute_binary = time(Minute), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'Second' ->
            NewBinaryStr = FunColor({Args, "Second_binary/binary"}),
            NewStrAST = "Second_binary = time(Second), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'Millisecond' ->
            NewBinaryStr = FunColor({Args, "MilliSecond_binary/binary"}),
            NewStrAST = "MilliSecond_binary = msecond_binary(Millisecond), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'Priority_cap' ->
            NewBinaryStr = FunColor({Args, "Priority_cap_binary/binary"}),
            NewStrAST = case FlagColor of
                            true ->
                                "Priority_cap_binary = Get_priority_prefix_up(PriorityInt), ";
                            false ->
                                "Priority_cap_binary = get_priority_prefix_up(PriorityInt), "
                        end,
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'Priority_short' ->
            NewBinaryStr = FunColor({Args, "Priority_short_binary/binary"}),
            NewStrAST = case FlagColor of
                            true ->
                                "Priority_short_binary = Get_priority_short_prefix(PriorityInt), ";
                            false ->
                                "Priority_short_binary = get_priority_short_prefix(PriorityInt), "
                        end,
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'Priority' ->
            NewBinaryStr = FunColor({Args, "Priority_binary/binary"}),
            NewStrAST = case FlagColor of
                            true ->
                                "Priority_binary = Get_priority_prefix_low(PriorityInt), ";
                            false ->
                                "Priority_binary = get_priority_prefix_low(PriorityInt), "
                        end,
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'Pid' ->
            NewBinaryStr = FunColor({Args, "Pid_binary/binary"}),
            NewStrAST = "Pid_binary = erlang:list_to_binary(erlang:pid_to_list(Pid)), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'File' ->
            NewBinaryStr = FunColor({Args, "File_binary/binary"}),
            NewStrAST = "File_binary = erlang:list_to_binary(File), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'Line' ->
            NewBinaryStr = FunColor({Args, "L_binary/binary"}),
            NewStrAST = "L_binary = line_binary(Line), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'Module' ->
            NewBinaryStr = FunColor({Args, "Module_binary/binary"}),
            NewStrAST = "Module_binary = erlang:list_to_binary(erlang:atom_to_list(Module)), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'Function' ->
            NewBinaryStr = FunColor({Args, "Function_binary/binary"}),
            NewStrAST = "Function_binary = erlang:list_to_binary(Function), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'UserStr' ->
            NewBinaryStr = FunColor({Args, "Message_binary/binary"}),
            NewStrAST = "Message_binary = unicode:characters_to_binary(UserStr), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        'UserStrLine' ->
            NewBinaryStr = FunColor({Args, "MessageLine_binary/binary"}),
            NewStrAST = "MessageLine_binary = unicode:characters_to_binary(UserStr), ",
            make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ ", \"\\n\"" ++ theend(Res), StrAST ++ NewStrAST, FunColor, FlagColor);
        _ ->
            make_format_function_ast(Res, BinaryStr ++ io_lib:format("~p",[[Args]]) ++ theend(Res), StrAST, FunColor, FlagColor)
    end.

theend([]) -> "";
theend(_) -> ", ".

set_color_colors_data(#chronica_colors_data{} = ColoringData, Data, Color) ->
    case Data of
        'Year'          -> ColoringData#chronica_colors_data{year_color = Color};
        'Month'         -> ColoringData#chronica_colors_data{month_color = Color};
        'Month_string'  -> ColoringData#chronica_colors_data{month_string_color = Color};
        'Day'           -> ColoringData#chronica_colors_data{day_color = Color};
        'Hour'          -> ColoringData#chronica_colors_data{hour_color = Color};
        'Minute'        -> ColoringData#chronica_colors_data{minute_color = Color};
        'Second'        -> ColoringData#chronica_colors_data{second_color = Color};
        'Millisecond'   -> ColoringData#chronica_colors_data{millisecond_color = Color};
        'Pid'           -> ColoringData#chronica_colors_data{pid_color = Color};
        'File'          -> ColoringData#chronica_colors_data{file_color = Color};
        'Line'          -> ColoringData#chronica_colors_data{line_color = Color};
        'Module'        -> ColoringData#chronica_colors_data{module_color = Color};
        'Function'      -> ColoringData#chronica_colors_data{function_color = Color};
        'UserStr'       -> ColoringData#chronica_colors_data{user_str_color = Color};
        'UserStrLine'   -> ColoringData#chronica_colors_data{user_str_line_color = Color};
        'Error'         -> ColoringData#chronica_colors_data{error_color = Color};
        'Warning'       -> ColoringData#chronica_colors_data{warning_color = Color};
        'Info'          -> ColoringData#chronica_colors_data{info_color = Color};
        'Debug'         -> ColoringData#chronica_colors_data{debug_color = Color};
        'Trace'         -> ColoringData#chronica_colors_data{trace_color = Color};
        'Priority_cap'  -> throw({not_apply_colot_to, Data});
        'Priority_short'-> throw({not_apply_colot_to, Data});
        'Priority'      -> throw({not_apply_colot_to, Data});
        _               -> throw({undefined_color_data, Data})
    end.

get_color_colors_data(#chronica_colors_data{} = ColoringData, Data) ->
    case Data of
        'Year'          -> ColoringData#chronica_colors_data.year_color;
        'Month'         -> ColoringData#chronica_colors_data.month_color;
        'Month_string'  -> ColoringData#chronica_colors_data.month_string_color;
        'Day'           -> ColoringData#chronica_colors_data.day_color;
        'Hour'          -> ColoringData#chronica_colors_data.hour_color;
        'Minute'        -> ColoringData#chronica_colors_data.minute_color;
        'Second'        -> ColoringData#chronica_colors_data.second_color;
        'Millisecond'   -> ColoringData#chronica_colors_data.millisecond_color;
        'Priority_cap'  -> ColoringData#chronica_colors_data.priority_cap_color;
        'Priority_short'-> ColoringData#chronica_colors_data.priority_short_color;
        'Priority'      -> ColoringData#chronica_colors_data.priority_color;
        'Pid'           -> ColoringData#chronica_colors_data.pid_color;
        'File'          -> ColoringData#chronica_colors_data.file_color;
        'Line'          -> ColoringData#chronica_colors_data.line_color;
        'Module'        -> ColoringData#chronica_colors_data.module_color;
        'Function'      -> ColoringData#chronica_colors_data.function_color;
        'UserStr'       -> ColoringData#chronica_colors_data.user_str_color;
        'UserStrLine'   -> ColoringData#chronica_colors_data.user_str_line_color;
        'Error'         -> ColoringData#chronica_colors_data.error_color;
        'Warning'       -> ColoringData#chronica_colors_data.warning_color;
        'Info'          -> ColoringData#chronica_colors_data.info_color;
        'Debug'         -> ColoringData#chronica_colors_data.debug_color;
        'Trace'         -> ColoringData#chronica_colors_data.trace_color;
        _               -> throw({undefined_colors_data, Data})
    end.

get_color([], Acc) -> Acc;
get_color([{foreground, Color} | Tail], Acc) ->
    get_color(Tail, concat_color(get_foreground_color(Color), Acc));
get_color([{background, Color} | Tail], Acc) ->
    get_color(Tail, concat_color(get_background_color(Color), Acc));
get_color([{bold, Bold} | Tail], Acc) ->
    get_color(Tail, concat_color(get_bold(Bold), Acc));
get_color([Head | _Tail], _Acc) ->
    throw({invalid_colors_spec, {invalid_param, Head}});
get_color(ColorsSpec, _Acc) ->
    throw({invalid_colors_spec, ColorsSpec}).

concat_color(Str1, Str2) ->
    case Str2 of
        []                  -> Str1;
        ?DEFAULT_COLOR_CODE -> Str1;
        _                   ->
            case Str1 of
                ?DEFAULT_COLOR_CODE -> Str2;
                _ -> Str1 ++ ";" ++ Str2
            end
    end.

get_bold(true)  -> ?BOLD_CODE;
get_bold(false) -> ?DEFAULT_COLOR_CODE;
get_bold(Other) ->
    throw({invalid_colors_spec, {invalid_bold, Other}}).

get_foreground_color(Color) ->
    case Color of
        red       -> ?FOREGROUND_RED_CODE;
        yellow    -> ?FOREGROUND_YELLOW_CODE;
        blue      -> ?FOREGROUND_BLUE_CODE;
        black     -> ?FOREGROUND_BLACK_CODE;
        green     -> ?FOREGROUND_GREEN_CODE;
        purple    -> ?FOREGROUND_PURPLE_CODE;
        cyan      -> ?FOREGROUND_CYAN_CODE;
        gray      -> ?FOREGROUND_GRAY_CODE;
        white     -> ?FOREGROUND_WHITE_CODE;
        default   -> ?DEFAULT_COLOR_CODE;
        _         -> throw({undefined_color, Color})
    end.

get_background_color(Color) ->
    case Color of
        red       -> ?BACKGROUND_RED_CODE;
        yellow    -> ?BACKGROUND_YELLOW_CODE;
        blue      -> ?BACKGROUND_BLUE_CODE;
        black     -> ?BACKGROUND_BLACK_CODE;
        green     -> ?BACKGROUND_GREEN_CODE;
        purple    -> ?BACKGROUND_PURPLE_CODE;
        cyan      -> ?BACKGROUND_CYAN_CODE;
        gray      -> ?BACKGROUND_GRAY_CODE;
        white     -> ?BACKGROUND_WHITE_CODE;
        default   -> ?DEFAULT_COLOR_CODE;
        _         -> throw({undefined_color, Color})
    end.

%include colors_data and partitioned 'DataTime' to data and time
update_colors_data([], #chronica_colors_data{} = Acc) -> Acc;
update_colors_data([Head | Tail], #chronica_colors_data{} = Acc) ->
    NewAcc =
        case Head of
        {'DataTime', Color} ->
            NewColor = get_color(Color, []),
            Acc#chronica_colors_data{
                year_color          = NewColor,
                month_color         = NewColor,
                month_string_color  = NewColor,
                day_color           = NewColor,
                hour_color          = NewColor,
                minute_color        = NewColor,
                second_color        = NewColor,
                millisecond_color   = NewColor
                };
        {Data, Color} ->
            set_color_colors_data(Acc, Data, get_color(Color, []))
        end,
    update_colors_data(Tail, NewAcc).

coloring_str(Str, ColorCode) ->
    case ColorCode of
        ?DEFAULT_COLOR_CODE -> Str;
        _ -> "\"\\e[" ++ ColorCode ++ "m\", " ++ Str ++ ", \"\\e[m\""
    end.

insert_tab(F) -> insert_tab_(lists:flatten(F), []).

insert_tab_([], Res) -> lists:reverse(Res);
insert_tab_([$%, $% | F], Res) ->
    {TabStr, Rest} = get_macro_name(F),
    try
        lists:reverse(Res) ++ Rest
    catch
        _:_ -> insert_tab_(Rest, lists:reverse(TabStr) ++ [$%, $%, Res])
    end;
insert_tab_([C | F], Res) -> insert_tab_(F, [C | Res]).

run([], Res, Mode, _FunColor) ->
   {Res, Mode};
run([$%, $% | F], [{Formated, Args} | Res], Mode, FunColor) ->
    run(F, [{Formated ++ [$%, $%], Args} | Res], Mode, FunColor);

run([$% | F], [{Formated, Args} | Res], Mode, FunColor) ->
    ?INT_DBG("found %, Tail: ~p", [F]),
    {MacroName, TailF} = get_macro_name(F),
    {VarName, FormatVar} = get_var_name(MacroName),
    NewMode = case {Mode, VarName} of
        {line, 'UserStr'} -> throw({bad_format, bad_mode, F});
        {message, 'UserStr'} -> throw({bad_format, bad_mode, F});
        {message, 'UserStrLine'} -> throw({bad_format, bad_mode, F});
        {line, 'UserStrLine'} -> throw({bad_format, bad_mode, F});
        {_, 'UserStr'} -> message;
        {_, 'UserStrLine'} -> line;
        _ -> Mode
    end,
    run(TailF, [{Formated ++ FormatVar, Args ++ [VarName]} | Res], NewMode, FunColor);

run([C | F], [{Formated, Args} | Res], Mode, FunColor) ->
    run(F, [{Formated , Args ++ [C]} | Res], Mode, FunColor).

get_macro_name(F) ->
    case re:run(F, "^(?<MACRO>\\w+)(?<TAIL>(.*(\\n)*.*)*)$", [{capture, ['MACRO', 'TAIL'], list}]) of
        {match, [N, Tail]} ->
            ?INT_DBG("matched macro: ~p and tail: ~p", [N, Tail]),
            {N, Tail};
        _ ->
            ?INT_ERR("Cant macth macro, tail: ~p", [F]),
            throw({error, get_macro_name, {bad_param, F}})
    end.

get_var_name("Y")           -> {'Year', "~4.10.0B"};
get_var_name("M")           -> {'Month', "~2.10.0B"};
get_var_name("Ml")          -> {'Month_string', "~s"};
get_var_name("D")           -> {'Day', "~2.10.0B"};
get_var_name("H")           -> {'Hour', "~2.10.0B"};
get_var_name("Mi")          -> {'Minute', "~2.10.0B"};
get_var_name("S")           -> {'Second', "~2.10.0B"};
get_var_name("Ms")          -> {'Millisecond', "~6.10.0B"};
get_var_name("PRIORITY")    -> {'Priority_cap', "~s"};
get_var_name("P")           -> {'Priority_short', "~s"};
get_var_name("Priority")    -> {'Priority', "~s"};
get_var_name("Pid")         -> {'Pid', "~w"};
get_var_name("File")        -> {'File', "~s"};
get_var_name("Line")        -> {'Line', "~b"};
get_var_name("Module")      -> {'Module', "~w"};
get_var_name("Function")    -> {'Function', "~s"};
get_var_name("Message")     -> {'UserStr', "~s"};
get_var_name("MessageLine") -> {'UserStrLine', "~s"};
get_var_name(Unknown) ->
    ?INT_DBG("Bad macro name: ~p", [Unknown]),
    throw({bad_macro, Unknown}).

frame(Data) -> << 16#AB, 16#CD, 16#EF, (erlang:size(Data)):32, Data/binary>>.

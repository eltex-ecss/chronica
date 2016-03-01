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

create_format_function(FunName, UserFormat,
        #chronica_coloring{colored = Colored,
                           formats_name = NameFormats,
                           colors_spec = ColorsSpec}) ->
    ?INT_DBG("create_format_function ~p ~s", [FunName, UserFormat]),

    {ListFUM_rev, _Mode} = run(UserFormat, [{[], []}], undef, fun (Other) -> Other end),
    ListFUM = lists:reverse(ListFUM_rev),
            ?INT_DBG("~p~n~n", [ListFUM]),
            FunArgs = fun({_, Args_c}, Acc) ->
                    Acc ++ Args_c
                end,
    Args = lists:foldl(FunArgs, [], ListFUM),
    {BinaryStr, FormattingStr} = case Colored of
        true ->
            case lists:keyfind(FunName, 2, NameFormats) of
                {_FormatId, FunName} ->
                    ColorsData = update_colors_data(ColorsSpec, #chronica_colors_data{}),
                    FunColor = fun ({Data, Str}) -> coloring_str(Str, get_color_colors_data(ColorsData, Data)) end,
                    make_format_function_ast(Args, "<<", "~p({{{Year,Month,Day},{Hour,Minute,Second},Millisecond}, PriorityInt, Module, Pid, Line, File, Function, UserStr}) -> " ++ priority_color(FunColor), FunColor, true);
                false ->
                    make_format_function_ast(Args, "<<", "~p({{{Year,Month,Day},{Hour,Minute,Second},Millisecond}, PriorityInt, Module, Pid, Line, File, Function, UserStr}) -> ", fun ({_Data, Str}) -> Str end, false)
            end;
        false ->
            make_format_function_ast(Args, "<<", "~p({{{Year,Month,Day},{Hour,Minute,Second},Millisecond}, PriorityInt, Module, Pid, Line, File, Function, UserStr}) -> ", fun ({_Data, Str}) -> Str end, false)
    end,
    ?INT_DBG("BinaryStr: ~p~n~n", [lists:flatten(BinaryStr)]),
    FunctionStr = FormattingStr ++ BinaryStr,
    ?INT_DBG("BinaryStr: ~s~n~n", [FunctionStr]),
    hd(pt_lib:str2ast(io_lib:format(FunctionStr, [FunName]), 0)).

priority_color(FunColor) ->
    "Get_priority_short_prefix =
        fun(1) -> <<" ++ FunColor({'Error', "\"*** ERROR\""}) ++ ">>;
           (2) -> <<" ++ FunColor({'Warning', "\"W\""}) ++ ">>;
           (3) -> <<" ++ FunColor({'Info', "\"I\""}) ++ ">>;
           (4) -> <<" ++ FunColor({'Trace', "\"T\""}) ++ ">>;
           (5) -> <<" ++ FunColor({'Debug', "\"D\""}) ++ ">> end,

    Get_priority_prefix_up =
        fun(1) -> <<" ++ FunColor({'Error', "\"ERROR\""}) ++ ">>;
           (2) -> <<" ++ FunColor({'Warning', "\"WARN \""}) ++ ">>;
           (3) -> <<" ++ FunColor({'Info', "\"INFO \""}) ++ ">>;
           (4) -> <<" ++ FunColor({'Trace', "\"TRACE\""}) ++ ">>;
           (5) -> <<" ++ FunColor({'Debug', "\"DEBUG\""}) ++ ">> end,

    Get_priority_prefix_low =
        fun(1) -> <<" ++ FunColor({'Error', "\"error\""}) ++ ">>;
           (2) -> <<" ++ FunColor({'Warning', "\"warning\""}) ++ ">>;
           (3) -> <<" ++ FunColor({'Info', "\"info\""}) ++ ">>;
           (4) -> <<" ++ FunColor({'Trace', "\"trace\""}) ++ ">>;
           (5) -> <<" ++ FunColor({'Debug', "\"debug\""}) ++ ">> end, ".

make_format_function_ast([], BinaryStr, StrAST, _FunColor, _FlagColor) ->
    {BinaryStr ++ ">>.", StrAST};
make_format_function_ast([Args = 'Year'| Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "Y_binary/binary"}),
    NewStrAST = "Y_binary = year_binary(Year), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'Month' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "Mo_binary/binary"}),
    NewStrAST = "Mo_binary = time(Month), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'Month_string' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "String_Mo_binary/binary"}),
    NewStrAST = "String_Mo_binary = month_str_binary(Month), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'Day' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "D_binary/binary"}),
    NewStrAST = "D_binary = time(Day), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'Hour' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "H_binary/binary"}),
    NewStrAST = "H_binary = time(Hour), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'Minute' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "Minute_binary/binary"}),
    NewStrAST = "Minute_binary = time(Minute), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'Second' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "Second_binary/binary"}),
    NewStrAST = "Second_binary = time(Second), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'Millisecond' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "MilliSecond_binary/binary"}),
    NewStrAST = "MilliSecond_binary = msecond_binary(Millisecond), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'Priority_cap' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "Priority_cap_binary/binary"}),
    NewStrAST = case FlagColor of
        true ->
            "Priority_cap_binary = Get_priority_prefix_up(PriorityInt), ";
        false ->
            "Priority_cap_binary = get_priority_prefix_up(PriorityInt), "
    end,
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'Priority_short' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "Priority_short_binary/binary"}),
    NewStrAST = case FlagColor of
        true ->
            "Priority_short_binary = Get_priority_short_prefix(PriorityInt), ";
        false ->
            "Priority_short_binary = get_priority_short_prefix(PriorityInt), "
    end,
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'Priority' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "Priority_binary/binary"}),
    NewStrAST = case FlagColor of
        true ->
            "Priority_binary = Get_priority_prefix_low(PriorityInt), ";
        false ->
            "Priority_binary = get_priority_prefix_low(PriorityInt), "
    end,
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'Pid' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "Pid_binary/binary"}),
    NewStrAST = "Pid_binary = erlang:list_to_binary(erlang:pid_to_list(Pid)), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'File' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "File_binary/binary"}),
    NewStrAST = "File_binary = erlang:list_to_binary(File), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'Line' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "L_binary/binary"}),
    NewStrAST = "L_binary = erlang:integer_to_binary(Line), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'Module' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "Module_binary/binary"}),
    NewStrAST = "Module_binary = erlang:list_to_binary(erlang:atom_to_list(Module)), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'Function' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "Function_binary/binary"}),
    NewStrAST = "Function_binary = erlang:list_to_binary(Function), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'UserStr' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "Message_binary/binary"}),
    NewStrAST = "Message_binary = unicode:characters_to_binary(UserStr), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args = 'UserStrLine' | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    NewBinaryStr = FunColor({Args, "MessageLine_binary/binary"}),
    NewStrAST = "MessageLine_binary = unicode:characters_to_binary(UserStr), ",
    make_format_function_ast(Res, BinaryStr ++ NewBinaryStr ++ ", \"\\n\"" ++ theend(Res),
                             StrAST ++ NewStrAST, FunColor, FlagColor);
make_format_function_ast([Args | Res], BinaryStr, StrAST, FunColor, FlagColor) ->
    make_format_function_ast(Res, BinaryStr ++ io_lib:format("~p",[[Args]]) ++ theend(Res),
                             StrAST, FunColor, FlagColor).

theend([]) -> "";
theend(_) -> ", ".

set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Year', Color) ->
    ColoringData#chronica_colors_data{year_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Month', Color) ->
    ColoringData#chronica_colors_data{month_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Month_string', Color) ->
    ColoringData#chronica_colors_data{month_string_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Day', Color) ->
    ColoringData#chronica_colors_data{day_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Hour', Color) ->
    ColoringData#chronica_colors_data{hour_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Minute', Color) ->
    ColoringData#chronica_colors_data{minute_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Second', Color) ->
    ColoringData#chronica_colors_data{second_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Millisecond', Color) ->
    ColoringData#chronica_colors_data{millisecond_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Pid', Color) ->
    ColoringData#chronica_colors_data{pid_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'File', Color) ->
    ColoringData#chronica_colors_data{file_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Line', Color) ->
    ColoringData#chronica_colors_data{line_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Module', Color) ->
    ColoringData#chronica_colors_data{module_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Function', Color) ->
    ColoringData#chronica_colors_data{function_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'UserStr', Color) ->
    ColoringData#chronica_colors_data{user_str_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'UserStrLine', Color) ->
    ColoringData#chronica_colors_data{user_str_line_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Error', Color) ->
    ColoringData#chronica_colors_data{error_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Warning', Color) ->
    ColoringData#chronica_colors_data{warning_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Info', Color) ->
    ColoringData#chronica_colors_data{info_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Debug', Color) ->
    ColoringData#chronica_colors_data{debug_color = Color};
set_color_colors_data(#chronica_colors_data{} = ColoringData, 'Trace', Color) ->
    ColoringData#chronica_colors_data{trace_color = Color};
set_color_colors_data(#chronica_colors_data{}, Data = 'Priority_cap', _Color) ->
    throw({not_apply_colot_to, Data});
set_color_colors_data(#chronica_colors_data{}, Data = 'Priority_short', _Color) ->
    throw({not_apply_colot_to, Data});
set_color_colors_data(#chronica_colors_data{}, Data = 'Priority', _Color) ->
    throw({not_apply_colot_to, Data});
set_color_colors_data(#chronica_colors_data{}, Data, _Color) ->
    throw({undefined_color_data, Data}).

get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Year') ->
    ColoringData#chronica_colors_data.year_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Month') ->
    ColoringData#chronica_colors_data.month_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Month_string') ->
    ColoringData#chronica_colors_data.month_string_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Day') ->
    ColoringData#chronica_colors_data.day_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Hour') ->
    ColoringData#chronica_colors_data.hour_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Minute') ->
    ColoringData#chronica_colors_data.minute_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Second') ->
    ColoringData#chronica_colors_data.second_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Millisecond') ->
    ColoringData#chronica_colors_data.millisecond_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Priority_cap') ->
    ColoringData#chronica_colors_data.priority_cap_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Priority_short') ->
    ColoringData#chronica_colors_data.priority_short_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Priority') ->
    ColoringData#chronica_colors_data.priority_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Pid') ->
    ColoringData#chronica_colors_data.pid_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'File') ->
    ColoringData#chronica_colors_data.file_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Line') ->
    ColoringData#chronica_colors_data.line_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Module') ->
    ColoringData#chronica_colors_data.module_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Function') ->
    ColoringData#chronica_colors_data.function_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'UserStr') ->
    ColoringData#chronica_colors_data.user_str_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'UserStrLine') ->
    ColoringData#chronica_colors_data.user_str_line_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Error') ->
    ColoringData#chronica_colors_data.error_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Warning') ->
    ColoringData#chronica_colors_data.warning_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Info') ->
    ColoringData#chronica_colors_data.info_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Debug') ->
    ColoringData#chronica_colors_data.debug_color;
get_color_colors_data(#chronica_colors_data{} = ColoringData, 'Trace') ->
    ColoringData#chronica_colors_data.trace_color;
get_color_colors_data(#chronica_colors_data{}, Data) ->
    throw({undefined_colors_data, Data}).

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

concat_color(Str1, []) -> Str1;
concat_color(Str1, ?DEFAULT_COLOR_CODE) -> Str1;
concat_color(?DEFAULT_COLOR_CODE, Str2) -> Str2;
concat_color(Str1, Str2) -> Str1 ++ ";" ++ Str2.

get_bold(true)  -> ?BOLD_CODE;
get_bold(false) -> ?DEFAULT_COLOR_CODE;
get_bold(Other) ->
    throw({invalid_colors_spec, {invalid_bold, Other}}).

get_foreground_color(red)     -> ?FOREGROUND_RED_CODE;
get_foreground_color(yellow)  -> ?FOREGROUND_YELLOW_CODE;
get_foreground_color(blue)    -> ?FOREGROUND_BLUE_CODE;
get_foreground_color(black)   -> ?FOREGROUND_BLACK_CODE;
get_foreground_color(green)   -> ?FOREGROUND_GREEN_CODE;
get_foreground_color(purple)  -> ?FOREGROUND_PURPLE_CODE;
get_foreground_color(cyan)    -> ?FOREGROUND_CYAN_CODE;
get_foreground_color(gray)    -> ?FOREGROUND_GRAY_CODE;
get_foreground_color(white)   -> ?FOREGROUND_WHITE_CODE;
get_foreground_color(default) -> ?DEFAULT_COLOR_CODE;
get_foreground_color(Color)   -> throw({undefined_color, Color}).

get_background_color(red)     -> ?BACKGROUND_RED_CODE;
get_background_color(yellow)  -> ?BACKGROUND_YELLOW_CODE;
get_background_color(blue)    -> ?BACKGROUND_BLUE_CODE;
get_background_color(black)   -> ?BACKGROUND_BLACK_CODE;
get_background_color(green)   -> ?BACKGROUND_GREEN_CODE;
get_background_color(purple)  -> ?BACKGROUND_PURPLE_CODE;
get_background_color(cyan)    -> ?BACKGROUND_CYAN_CODE;
get_background_color(gray)    -> ?BACKGROUND_GRAY_CODE;
get_background_color(white)   -> ?BACKGROUND_WHITE_CODE;
get_background_color(default) -> ?DEFAULT_COLOR_CODE;
get_background_color(Color)   -> throw({undefined_color, Color}).

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

coloring_str(Str, ?DEFAULT_COLOR_CODE) -> Str;
coloring_str(Str, ColorCode) ->
        "\"\\e[" ++ ColorCode ++ "m\", " ++ Str ++ ", \"\\e[m\"".

insert_tab(F) -> insert_tab_(lists:flatten(F), []).

insert_tab_([], Res) -> lists:reverse(Res);
insert_tab_([$%, $% | F], Res) ->
    {TabStr, Rest} = get_macro_name(F),
    try
        lists:reverse(Res, Rest)
    catch
        _:_ -> insert_tab_(Rest, lists:reverse(TabStr, [$%, $%, Res]))
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

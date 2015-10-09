%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-define(DEFAULT_COLOR_CODE, "0").

-define(FOREGROUND_BLACK_CODE, "30").
-define(FOREGROUND_RED_CODE, "31").
-define(FOREGROUND_GREEN_CODE, "32").
-define(FOREGROUND_YELLOW_CODE, "33").
-define(FOREGROUND_BLUE_CODE, "34").
-define(FOREGROUND_PURPLE_CODE, "35").
-define(FOREGROUND_CYAN_CODE, "36").
-define(FOREGROUND_GRAY_CODE, "37").
-define(FOREGROUND_WHITE_CODE, "38").

-define(BACKGROUND_BLACK_CODE, "40").
-define(BACKGROUND_RED_CODE, "41").
-define(BACKGROUND_GREEN_CODE, "42").
-define(BACKGROUND_YELLOW_CODE, "43").
-define(BACKGROUND_BLUE_CODE, "44").
-define(BACKGROUND_PURPLE_CODE, "45").
-define(BACKGROUND_CYAN_CODE, "46").
-define(BACKGROUND_GRAY_CODE, "47").
-define(BACKGROUND_WHITE_CODE, "48").

-define(BOLD_CODE, "1").

-type chronica_color() :: string().

-record(chronica_colors_data,
{
    year_color          = ?FOREGROUND_YELLOW_CODE :: chronica_color(),
    month_color         = ?FOREGROUND_YELLOW_CODE :: chronica_color(),
    month_string_color  = ?FOREGROUND_YELLOW_CODE :: chronica_color(),
    day_color           = ?FOREGROUND_YELLOW_CODE :: chronica_color(),
    hour_color          = ?FOREGROUND_YELLOW_CODE :: chronica_color(),
    minute_color        = ?FOREGROUND_YELLOW_CODE :: chronica_color(),
    second_color        = ?FOREGROUND_YELLOW_CODE :: chronica_color(),
    millisecond_color   = ?FOREGROUND_YELLOW_CODE :: chronica_color(),
    priority_cap_color  = ?DEFAULT_COLOR_CODE :: chronica_color(),
    priority_short_color= ?DEFAULT_COLOR_CODE :: chronica_color(),
    priority_color      = ?DEFAULT_COLOR_CODE :: chronica_color(),
    pid_color           = ?BOLD_CODE :: chronica_color(),
    file_color          = ?FOREGROUND_GRAY_CODE :: chronica_color(),
    line_color          = ?FOREGROUND_GREEN_CODE :: chronica_color(),
    module_color        = ?FOREGROUND_CYAN_CODE :: chronica_color(),
    function_color      = ?BOLD_CODE :: chronica_color(),
    user_str_color      = ?DEFAULT_COLOR_CODE :: chronica_color(),
    user_str_line_color = ?DEFAULT_COLOR_CODE :: chronica_color(),
    error_color         = ?BACKGROUND_RED_CODE ++ ";" ++ ?FOREGROUND_WHITE_CODE ++ ";" ++ ?BOLD_CODE :: chronica_color(),
    warning_color       = ?FOREGROUND_RED_CODE ++ ";" ++ ?BOLD_CODE :: chronica_color(),
    info_color          = ?FOREGROUND_WHITE_CODE :: chronica_color(),
    debug_color         = ?FOREGROUND_GRAY_CODE :: chronica_color(),
    trace_color         = ?FOREGROUND_GREEN_CODE ++ ";" ++ ?BOLD_CODE :: chronica_color()
}).
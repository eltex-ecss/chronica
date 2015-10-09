%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Anton N Ryabkov
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%% Created : 22. Апр. 2015 11:30
%%%-------------------------------------------------------------------
-module(chronica_parser).
-include("chronica_parser.hrl").

-export([tokenize_format_string/1]).

tokenize_format_string(Input) ->
    tokenize_format_string(Input, []).

tokenize_format_string("", Tokens) ->
    lists:reverse(Tokens);

tokenize_format_string(Input, Tokens) ->
    case parse_control_sequence_token(Input) of
        {Input2, Token} ->
            tokenize_format_string(Input2, [#token{type = control, value = Token} | Tokens]);
        {unknown_control, ControlStr, Input2} ->
            {Input3, Token3} = parse_pure_string_token(Input2, lists:reverse(ControlStr)),
            tokenize_format_string(Input3, [#token{type = string, value = Token3} | Tokens]);
        control_not_found ->
            {Input3, Token3} = parse_pure_string_token(Input, ""),
            tokenize_format_string(Input3, [#token{type = string, value = Token3} | Tokens])
    end.

parse_pure_string_token("", Token) ->
    {"", lists:reverse(Token)};

parse_pure_string_token([$%, $s, $t, $a, $c, $k | Input], Token) ->
    {[$~, $S | Input], lists:reverse(Token)};

parse_pure_string_token([$~, $~ | Input], Token) ->
    parse_pure_string_token(Input, [$~, $~ | Token]);

parse_pure_string_token([$~, $n | Input], Token) ->
    parse_pure_string_token(Input, [$n, $~ | Token]);

parse_pure_string_token([$~ | Input], Token) ->
    {[$~ | Input], lists:reverse(Token)};

parse_pure_string_token([Char | Input], Token) ->
    parse_pure_string_token(Input, [Char | Token]).

parse_control_sequence_token([$~ | Input1]) ->
    {Input2, F} = parse_control_sequence_field_width(Input1, ""),
    {Input3, P} = parse_control_sequence_precision(Input2, ""),
    {Input4, Pad} = parse_control_sequence_padding(Input3),
    {Input5, Mod} = parse_control_sequence_mod(Input4),
    case parse_control_sequence_control(Input5) of
        {Input6, C} ->
            {Input6, lists:append([[$~], F, P, Pad, Mod, C])};
        {unknown_control, C, Input6} ->
            {unknown_control, lists:append([[$~], F, P, Pad, Mod, C]), Input6}
    end;

parse_control_sequence_token(_Input) ->
    control_not_found.

parse_control_sequence_field_width([Char | Input], Token) when (Char >= $0 andalso Char =< $9) orelse Char =:= $+ orelse Char =:= $- ->
    parse_control_sequence_field_width(Input, [Char | Token]);

parse_control_sequence_field_width(Input, Token) ->
    {Input, lists:reverse(Token)}.

parse_control_sequence_precision([$., Char | Input], "") when (Char >= $0 andalso Char =< $9) orelse Char =:= $+ orelse Char =:= $- ->
    parse_control_sequence_precision(Input, [Char, $.]);

parse_control_sequence_precision([$., $. | Input], "") ->
    parse_control_sequence_precision([$. | Input], [$.]);

parse_control_sequence_precision(Input, "") ->
    {Input, ""};

parse_control_sequence_precision([Char | Input], Token) when (Char >= $0 andalso Char =< $9) orelse Char =:= $+ orelse Char =:= $- ->
    parse_control_sequence_precision(Input, [Char | Token]);

parse_control_sequence_precision(Input, Token) ->
    {Input, lists:reverse(Token)}.

parse_control_sequence_padding([$., Char | Input]) ->
    {Input, [$., Char]};

parse_control_sequence_padding(Input) ->
    {Input, ""}.

parse_control_sequence_mod([Char | Input]) when Char =:= $t orelse Char =:= $l ->
    {Input, [Char]};

parse_control_sequence_mod(Input) ->
    {Input, ""}.

parse_control_sequence_control([Char | Input]) when Char =:= $c orelse Char =:= $f orelse Char =:= $e orelse
    Char =:= $g orelse Char =:= $s orelse Char =:= $w orelse
    Char =:= $p orelse Char =:= $W orelse Char =:= $P orelse
    Char =:= $B orelse Char =:= $X orelse Char =:= $# orelse
    Char =:= $b orelse Char =:= $x orelse Char =:= $+ orelse
    Char =:= $i orelse Char =:= $S ->
    {Input, [Char]};

parse_control_sequence_control([Char | Input]) ->
    {unknown_control, [Char], Input};

parse_control_sequence_control(Input) ->
    {unknown_control, "", Input}.
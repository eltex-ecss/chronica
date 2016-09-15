%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_tcp_con_backend).

-include("chronica_int.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").

-behaviour(chronica_gen_backend).

-export([handle_open/3, handle_close/1, handle_write/3, handle_clear/1, handle_rotate/1, handle_check/1]).

handle_open(Con, _, Files) when is_pid(Con) ->
    {ok, erlang:term_to_binary(Con), Files};
handle_open(Con, _, _) -> {error, {bad_con, Con, not_pid}}.

handle_close(Con) ->
    try
        chronica_tcp_connection:close(erlang:binary_to_term(Con), normal)
    catch
        _C:E -> {error, E}
    end.

handle_write(Con, Data, TypeFormat) when TypeFormat =:= binary ->
    try
        chronica_tcp_connection:write(erlang:binary_to_term(Con), chronica_format_creator:frame(Data))
    catch
        _C:E -> {error, E}
    end;

handle_write(Con, Str, _TypeFormat) ->
    try
        chronica_tcp_connection:write(erlang:binary_to_term(Con), Str)
    catch
        _C:E -> {error, E}
    end.

handle_clear(_Handle) -> ok.

handle_rotate(_Handle) -> ok.

handle_check(Con) ->
    case erlang:binary_to_term(Con) of
        ConPid when is_pid(ConPid) ->
            erlang:is_process_alive(ConPid);
        _ ->
            false
    end.

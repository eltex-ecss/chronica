%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_udp_backend).

-behaviour(gen_server).
-behaviour(chronica_gen_backend).

-include("chronica_int.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").

-export(
   [
    handle_open/3,
    handle_close/1,
    handle_write/3,
    handle_clear/1,
    handle_rotate/1,
    handle_check/1
   ]).

-export(
   [
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

handle_open({StrIP, Port}, _, Files) ->
    try
        case gen_udp:open(?DEFAULT_LOCAL_UDP_PORT, [inet, list, {active, false}]) of
            {ok, Socket} ->
                case start_link({Socket, StrIP, Port}) of
                    {ok, Name} ->
                        OpenMsg = io_lib:format("Open UDP log from ~p log to ~p:~p~n", [node(), StrIP, Port]),
                        handle_write(Name, OpenMsg, default),
                        {ok, Name, Files};
                    {error, Reason} ->
                        ?INT_ERR("Error open udp socket(~p:~p) cause cant start gen_server: ~p", [StrIP, Port, Reason]),
                        {error, Reason}
                end;
            {error, Reason} ->
                ?INT_ERR("Error open udp socket(~p:~p) with reason ~p", [StrIP, Port, Reason]),
                {error, Reason}
        end
    catch
        _C:E ->
            {error, E}
    end.

handle_close(Name) ->
    try
        handle_write(Name, io_lib:format("Close UDP log from ~p", [node()]), default),
        gen_server:cast(Name, {close, normal})
    catch
        _C:E ->
            {error, E}
    end.

handle_write(Name, Str, TypeFormat) ->
    try
        gen_server:cast(Name, {write, Str, TypeFormat})
    catch
        _C:E ->
            {error, E}
    end.

handle_clear(_Handle) ->
    ok.

handle_rotate(_Handle) -> ok.

handle_check(Handle) when is_atom(Handle) ->
    erlang:whereis(Handle) =/= undefined;

handle_check(Handle) when is_pid(Handle) ->
    erlang:is_process_alive(Handle);

handle_check(_Handle) ->
    false.

createName(Ip, Port) ->
    erlang:list_to_atom(Ip ++ "_" ++ integer_to_list(Port)).

start_link({Socket, Ip, Port}) ->
    Name = createName(Ip, Port),
    case gen_server:start_link({local, createName(Ip, Port)}, ?MODULE, {Socket, Ip, Port}, []) of
        {ok, _Pid} -> {ok, Name};
        ignore -> {error, ignore};
        {error, {already_started, _Pid}} -> {ok, Name};
        {error, Reason} -> {error, Reason}
    end.

init({Socket, Ip, Port}) ->
    {ok, {Socket, Ip, Port}}.

handle_cast({write, Mess, TypeFormat}, State = {Socket, IP, Port}) when TypeFormat =:= binary ->
    Res = gen_udp:send(Socket, IP, Port, chronica_format_creator:frame(Mess)),
    case Res of
       {error, _Reason} -> ?INT_ERR("Error write returned ~p", [Res]);
       ok -> ok
    end,
    {noreply, State};

handle_cast({write, Mess, _TypeFormat}, State = {Socket, IP, Port}) ->
    Res = gen_udp:send(Socket, IP, Port, Mess),
    case Res of
       {error, _Reason} -> ?INT_ERR("Error write returned ~p", [Res]);
       ok -> ok
    end,
    {noreply, State};
handle_cast({close, Reason}, State = {Socket, _, _}) ->
    gen_udp:close(Socket),
    {stop, Reason, State};
handle_cast(_Unknown, State) ->
    ?INT_ERR("Unhandled cast request ~p", [_Unknown]),
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    ?INT_ERR("Unhandled call request~p", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    ?INT_ERR("Unhandled info ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

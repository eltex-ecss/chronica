%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_disk_log_police).

-include_lib("chronica_int.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").

-export([start/1, stop/0, loop/1]).

start(Timeout) ->
    case whereis(?MODULE) of
        undefined ->
            Pid = spawn(?MODULE, loop, [Timeout]),
            register(?MODULE, Pid),
            ok;
        _Pid -> ok
    end.

stop() ->
    ?MODULE ! stop,
    ok.

loop(Timeout) ->
    receive
        stop -> ok
    after
        Timeout ->
            police(),
            loop(Timeout)
    end.

police() ->
    ?INT_DBG("pulse~n", []),
    {ListOfLogs, _} = chronica_disk_log:accessible_logs(),
    check_file_exists(ListOfLogs).

check_file_exists([]) ->
    ok;
check_file_exists([Log | Tail]) ->
    case catch chronica_disk_log:info(Log) of
        [_, {file, Name}, {type, wrap}, {format, external} | _] ->
            case filelib:is_regular(Name) of
                false ->
                    ?INT_DBG("Recreating log ~p(~p)", [Log, Name]),
                    chronica_disk_log:inc_wrap_file(Log);
                true -> ok
            end;
        _ ->
            ok
    end,
    check_file_exists(Tail).

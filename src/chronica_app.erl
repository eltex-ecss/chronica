%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin, Anton N Ryabkov
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_app).

-behaviour(application).

-export([start/2, stop/1, prep_stop/1]).

-include_lib("chronica_config.hrl").
-include_lib("pt_scripts/include/pt_versioned.hrl").

%%====================================================================
%% Application callbacks
%%====================================================================
start(_Type, StartArgs) ->
    io:format("Starting Chronica... ~n", []),
    Now = os:timestamp(),

    Config = case chronica_manager:read_config(Now) of
        {ok, Cfg} ->
            NewBckends = Cfg#chronica_config.backend_modules ++
                [{file, chronica_file_backend}],
            Cfg#chronica_config{backend_modules = NewBckends};
        Err ->
            io:format("failed, 'cause cant read config: ~p~n", [Err]),
            throw(Err)
    end,

    ILParams = Config#chronica_config.internal_logger,
    TCPPort = Config#chronica_config.tcp_port,
    TCPHost = Config#chronica_config.tcp_host,
    CacheDir = Config#chronica_config.data_root,

    case filelib:ensure_dir(filename:join(CacheDir, "filename")) of
        ok -> ok;
        {error, EnsureError} ->
            io:format("failed, can't create dir for cache: ~p, reason: ~p", [CacheDir, EnsureError]),
            throw({error, EnsureError})
    end,


    Args = [{now, Now}, {config, Config}, {internal_logger, ILParams},
            {tcp_port, TCPPort}, {tcp_host, TCPHost} | StartArgs],
    SupPid = case chronica_supervisor:start_link(Args) of
        {ok, Pid} -> Pid;
        Error ->
            io:format("failed, reason: ~p~n", [Error]),
            throw(Error)
    end,
    chronica_manager:init_sync(),

    io:format("ok~n"),
    (not Config#chronica_config.tty_enabled) andalso io:format("WARNING: TTY output is disabled~n"),
    case application:get_env(chronica, auto_testing) of
        {ok, true} -> chronica_testing:auto();
        _ -> ok
    end,
    {ok, SupPid}.

prep_stop(_) ->
    ok.

stop(_State) ->
    io:format("Stopping Chronica application...~n", []),
    chronica_internal_logger:close(),
    io:format("ok~n", []),
    ok.

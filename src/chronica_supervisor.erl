%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include("chronica_int.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
start_link(Param) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Param).

%%====================================================================
%% Supervisor callbacks

% [{now, Now}, {internal_logger_params, ILParams}, {config, Config}]
init(ParamList) ->
    try
        ILParams =
            case lists:keyfind(internal_logger, 1, ParamList) of
                {internal_logger, ILP} -> ILP;
                _ -> throw({bad_start_param, internal_logger_params})
            end,

        TCPPort =
            case lists:keyfind(tcp_port, 1, ParamList) of
                false     -> undefined;
                {_, Port} -> Port
            end,

        TCPHost =
            case lists:keyfind(tcp_host, 1, ParamList) of
                false     -> any;
                {_, Host} -> Host
            end,

        case chronica_internal_logger:init(ILParams) of
            ok ->
                LogConfigurator =
                    {chronica_manager, {chronica_manager, start_link, [ParamList]}, permanent, 2000, worker, []},

                ErrorBuffer = {chronica_error_buffer, {cycled_events, start_link, [chronica_error_buffer, ?MAX_ERROR_BUFFER_LEN]}, temporary, 1000, worker, []},

                TCPServer = case TCPPort of
                        undefined -> [];
                        _ ->
                            [
                             {chronica_tcp_server,
                              {chronica_tcp_server, start_link,
                               [TCPHost, TCPPort]}, permanent, 2000, worker, []
                             }
                            ]
                    end,
                {ok, {{one_for_one, 1, 1},
                     [
                      ErrorBuffer,
                      LogConfigurator
                     ] ++ TCPServer
                    }};
            Err ->
                io:format("Log Sever internal logger start failed cause: ~p~n", [Err]),
                Err
        end
    catch
        throw:Error -> Error
    end.

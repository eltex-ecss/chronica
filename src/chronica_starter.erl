%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Teplyashin Andrey
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%     API for simple start chronica logger from tests
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_starter).

-export([
          start/0,
          start/4,
          disable_sasl/0,
          mocking/0
         ]).

-include("chronica_config.hrl").

%% @doc start()
%% Function for simple manual start chronica (for example of tests)
%% @note Loggered application must loaded befor call chronica:starter:start()
%% for correct worked.
start() ->
    start(debug, "*", short, []).

-spec start(Priority :: debug | trace | info | warning | error,
            Mask :: string(), % use &,*,?,|,!
            FormatLog :: default | short | html,
            ListManualStartApp :: [atom() | {application, atom()}]) ->
    ignore | {error, _Reason} | {ok, pid()}.
start(Priority, Mask, FormatLog, ListManualStartApp) ->
    [ application:load(App) || App <- ListManualStartApp ],
    {ok, _} = application:ensure_all_started(chronica),
    Now = os:timestamp(),
    Config = #chronica_config{
                rules =
                [
                 #chronica_rule{
                    id = Priority,
                    mask = Mask ++ "&!chronica_error_logger_handler",
                    priority = debug,
                    flow_ids = [flow1],
                    in_work = true}
                ],
                flows =
                [
                 #chronica_flow{
                    flow_id = flow1,
                    backends =
                    [
                     #chronica_backend{
                        type = {tty, undefined},
                        format = FormatLog
                       }
                    ]
                   }
                ],
                formats =
                [
                 #chronica_format{
                    format_id = default,
                    format = "%Y-%M-%D %H:%Mi:%S:%Ms %PRIORITY %Pid [%Module:%Line]: %Message\n"
                   },
                 #chronica_format{
                    format_id = short,
                    format = "%Mi:%S:%Ms %PRIORITY %Pid [%Module:%Line]: %Message\n"
                   }
                 #chronica_format{
                    format_id = html,
                    format = "%Mi:%S:%Ms <mark>%PRIORITY</mark> %Pid <b>[%Module:%Line]:</b> %Message\n"
                   }
                ],
                active = true,
                rotate_at_start = true,
                log_root = "../test/log/",
                internal_logger = [{tty, error}]
            },
    ILParams = Config#chronica_config.internal_logger,
    chronica_supervisor:start_link([{now, Now}, {config, Config},
        {internal_logger, ILParams}, {cache_dir, "/tmp"}]).

disable_sasl() ->
    application:load(sasl),
    application:set_env(sasl, sasl_error_logger, false).

mocking() ->
    application:set_env(lager, error_logger_redirect, false),
    application:set_env(lager, handlers, []),
    meck:new(chronica_core, [passthrough]),
    meck:expect(chronica_core, log_fast,
                fun(_1, _2, _3, _4, _5, _6, _7, _8, _9) when is_function(_9) ->
                        io:format(">"++_8++"~n", _9());
                   (_1, _2, _3, _4, _5, _6, _7, _8, _9) ->
                        io:format(">"++_8++"~n", _9)
                end),
    meck:new(trike_context_api, [passthrough]),
    meck:expect(trike_context_api, get_env_prop, fun([domain, name]) -> "d.112" end),
    meck:expect(trike_context_api, uuid, fun() -> <<"UUID STUB">> end).
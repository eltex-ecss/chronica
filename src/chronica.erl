%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica).

-dialyzer({no_match, register/1}).

-include("chronica_int.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").
-include("chronica_config.hrl").

-export([
    clear_log/1,
    clear_log/0,
    activate/0,
    deactivate/0,
    rotate/0,
    register/1,
    register/0,
    load_config/1,
    add_rule/4,
    generate_iface_module/1,
    get_tty_config/0
    ]).


-spec register() -> ok | {error, term()}.

register() ->
    case application:get_application(self()) of
        {ok, App} ->
            register(App);
        _ ->
            {error, no_application}
    end.

-spec register(atom()) -> no_return().
register(App) ->
    true = chronica_status:configured(),
    chronica_manager:add_application(App).

activate() ->
    run_try_manager(active, [true]).

deactivate() ->
    run_try_manager(active, [false]).

-spec add_rule(NameRule :: atom(), Mask :: nonempty_string(),
    Priority :: chronica_priority(), Flow :: atom()) -> ok | {error, _Reason}.
add_rule(NameRule, Mask, Priority, Flow) ->
    run_try_manager(add_rule, [NameRule, Mask, Priority, Flow]).

clear_log(FlowId) ->
    run_try_manager(clear_log, [FlowId]).

clear_log() ->
    clear_log('').

rotate() ->
    run_try_manager(rotate, []).

load_config(Config) ->
    run_try_manager(load_config, [Config]).

generate_iface_module(Module) ->
    run_try_manager(generate_iface_module, [Module]).

get_tty_config() ->
    #chronica_config{
        rules = [#chronica_rule{mask = '*', priority = info, flow_ids = [default]}],
        flows = [#chronica_flow{flow_id = default, backends = [#chronica_backend{type = tty, format = default}]}],
        formats = [],
        active = true,
        detail_info = false,
        rotate_at_start = false,
        internal_logger = [{tty, warning}],
        log_root = "./log/",
        data_root = "./cache/",
        max_file_size = 1024*1024*10,
        max_file_num = 10,
        tty_enabled = true,
        tcp_port = undefined,
        tcp_host = any,
        backend_modules = []
      }.

run_try_manager(Function, Args) ->
    try
        erlang:apply(chronica_manager, Function, Args)
    catch
        C:E ->
            ?INT_EXCEPT("Exception: ~p:~p", [C, E]),
            {error, E}
    end.

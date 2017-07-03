%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin, Anton N Ryabkov, Nikita Roshchupkin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_manager).

-behaviour(gen_server).

-include("chronica_int.hrl").
-include("chronica_config.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").
-include_lib("pt_lib/include/pt_lib.hrl").

-export(
   [
    active/1,
    add_application/1,
    add_rule/4,
    add_rule/5,
    add_tcp_connection/5,
    clear_log/1,
    free_resources/0,
    get_config/0,
    get_data_dir/0,
    get_flow_list/0,
    get_module_num/0,
    get_root_dir/0,
    init_sync/0,
    load_config/1,
    remove_tcp_connection/2,
    rotate/0,
    test_add_module/1,
    update_rule_inwork/2,
    update_rule_inwork/3
   ]).

-export(
   [
    init/1,
    start_link/1,
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
   ]).

-export_type(
   [
    rule/0
   ]
  ).

-define(wait_before_close_output_at_cfg_reload_timeout, 300000).
-define(wait_before_close_output_at_stop_timeout, 0).
-define(save_cache_timeout, 10000).
-define(check_log_backend_timeout, 60000).


-record(config_state,
    {
        loaded_config :: #chronica_config{},
        rules :: [#rule{}] | null,
        flows :: [term()] | null,
        create_time :: {integer(), integer(), integer()},
        registered_applications :: sets:set(module()),
        cache = [],
        config_hash = undefined,
        cache_timer = false
    }).

-record(initialize_sync, {}).

start_link(Params) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, [{spawn_opt, [{priority, low}]}]).

get_flow_list() ->
    catch gen_server:call(?MODULE, get_flow_list).

get_root_dir() ->
    gen_server:call(?MODULE, get_root_dir).

get_data_dir() ->
    gen_server:call(?MODULE, get_data_dir).

add_tcp_connection(Mask, Priority, Type, Con, Continuation) ->
    gen_server:cast(?MODULE, {add_tcp_connection, Mask, Priority, Type, Con, Continuation}).

remove_tcp_connection(Rule, Con) ->
    gen_server:cast(?MODULE, {remove_tcp_connection, Rule, Con}).

init_sync() ->
    gen_server:call(?MODULE, #initialize_sync{}, infinity).

init(Params) ->
    try
        ?INT_DBG("Starting chronica_manager", []),

        erlang:process_flag(trap_exit, true),

        Now =
            case proplists:get_value(now, Params, undefined) of
                undefined ->
                    ?INT_ERR("There is no \"now\" param in parameters to chronica_manager", []),
                    throw(no_now);
                N -> N
            end,

        State =
            #config_state{
                loaded_config = #chronica_config{},
                rules = null,
                flows = null,
                create_time = Now,
                registered_applications = sets:from_list([chronica])
                },

        NewState =
            case proplists:get_value(config, Params, undefined) of
                undefined ->
                    ?INT_ERR("No config in init params", []),
                    throw(no_config);

                C ->
                    ?INT_DBG("Init with config: ~p", [C]),
                    case start(State, C) of
                        {ok, NState} ->
                            ?INT_DBG("Inited with State: ~p", [NState]),
                            gen_event:add_handler(error_logger, chronica_error_logger_handler, []),
                            gen_server:cast(?MODULE, cache_empty),
                            NState;
                        Err ->
                            ?INT_ERR("Error when starting chronica_manager: ~p", [Err]),
                            throw(Err)
                    end
            end,
        % Первый раз проверяем backend-ы после трех timeout-ов после старта системы.
        timer:send_after(3 * ?check_log_backend_timeout, check_log_backend),
        {ok, NewState}
    catch
        throw:E -> {stop, E};
        _:E ->
            ?INT_ERR("manager failed to start: ~p~n~p", [E, erlang:get_stacktrace()]),
            {stop, E}
    end.

handle_cast({add_tcp_connection, Mask, Priority, Type, Con, Continuation},
        State = #config_state{loaded_config =
                    Config = #chronica_config{rules = Rules, flows = Flows}}) ->
    Name = erlang:list_to_atom(generate_name("tcp_")),
    FlowName = erlang:list_to_atom(generate_name("tcp_flow_")),
    NewRule = #chronica_rule{id = Name, mask = Mask,
                             priority = erlang:list_to_atom(Priority),
                             flow_ids = [FlowName], in_work = true},
    Backends = [#chronica_backend{type = {tcp_con, Con}, format =
                                  erlang:list_to_atom(Type)}],
    NewFlow = #chronica_flow{flow_id = FlowName, backends = Backends},
    NewConfig = Config#chronica_config{rules = [NewRule|Rules], flows = [NewFlow|Flows]},

    case catch true_load_config(State, NewConfig) of
        {ok, NewState} ->
            Continuation({ok, {Name, FlowName}}),
            {noreply, NewState};
        {error, NewState, Reason} ->
            Continuation({error, Reason}),
            {noreply, NewState};
        {stop, Reason} ->
            Continuation({error, Reason}),
            {stop, Reason, State};
        Err ->
            Continuation({error, Err}),
            {stop, Err, State}
    end;

handle_cast({remove_tcp_connection, {Name, FlowId}, _Con},
            State = #config_state{loaded_config = Config =
                                  #chronica_config{rules = Rules, flows = Flows}}) ->
    case lists:keytake(FlowId, #chronica_flow.flow_id, Flows) of
        false -> {noreply, State};
        {_value, _, NewFlows} ->
            NewRules = lists:keydelete(Name, #chronica_rule.id, Rules),
            NewConfig = Config#chronica_config{rules = NewRules, flows = NewFlows},

            case catch true_load_config(State, NewConfig) of
                {ok, NewState} -> {noreply, NewState};
                {error, NewState, _Reason} -> {noreply, NewState};
                {stop, Reason} -> {stop, Reason,  State};
                Err -> {stop, Err, State}
            end
    end;

handle_cast(cache_empty, State) ->
    set_configured_true(),
    {noreply, State};

handle_cast(_Unknown, State) ->
    ?INT_ERR("Unhandled cast request ~p", [_Unknown]),
    {noreply, State}.

handle_call(#initialize_sync{}, _Flows, State = #config_state{
        registered_applications = RApps, rules = Rules,
        loaded_config = #chronica_config{data_root = CacheDir, detail_info = Detail_info} = Config,
        cache_timer = IsCacheTimerStarted}) ->
    ConfigHash = config_hash(Config),
    Cache = chronica_cache:load(CacheDir, ConfigHash),

    AppListToRegister = application:loaded_applications(),
    AddApplicationFun =
        fun({AppL, _, _}, {RAppsL, CacheL}) ->
            {_, {RAppsL2, CacheL2}} = add_application(AppL, RAppsL, Rules, CacheL, Detail_info),
            {RAppsL2, CacheL2}
        end,
    {NewRApps, NewCache} = lists:foldl(AddApplicationFun, {RApps, Cache}, AppListToRegister),
    NewIsCacheTimerStarted =
        case NewCache == Cache of
            true  ->
                IsCacheTimerStarted;
            false ->
                IsCacheTimerStarted orelse timer:send_after(?save_cache_timeout, save_cache),
                true
        end,
    {reply, ok, State#config_state{registered_applications = NewRApps,
        cache = NewCache, config_hash = ConfigHash, cache_timer = NewIsCacheTimerStarted}};

handle_call(get_root_dir, _Flows, State = #config_state{loaded_config =
                                    #chronica_config{log_root = LogRoot}}) ->
    {reply, LogRoot, State};

handle_call(get_data_dir, _Flows, State = #config_state{loaded_config =
                                    #chronica_config{data_root = DataRoot}}) ->
    {reply, DataRoot, State};

handle_call({load_config, Config}, _From, State) ->
    ?INT_DBG("load_config received", []),

    case catch true_load_config(State, Config) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, NewState, Reason} -> {reply, {error, Reason}, NewState};
        {stop, Reason} -> {stop, Reason, State};
        Err -> {stop, Err, State}
    end;

handle_call({get_flows, Tags, Priority} = Msg, _From, State = #config_state{rules = Rules}) ->
    ?INT_DBG("get flows: Msg = ~p, Rules = ~w", [Msg, Rules]),
    case Rules of
        null -> {reply, [], State};
        _ ->
            UTags = lists:usort(Tags),
            Res = chronica_iface:get_appropriate_flows(UTags, Priority, Rules),
            {reply, Res, State}

    end;

handle_call(get_flow_list, _From, State = #config_state{flows = Flows}) ->
    {reply, {ok, [erlang:element(1, F) || F <- Flows]}, State};

handle_call({clear_log, _RuleId}, _From, State = #config_state{loaded_config =
                                    #chronica_config{active = false}}) ->
    {reply, {error, deactivated}, State};

handle_call({clear_log, RuleId} = Msg, _From, State = #config_state{flows = Flows}) ->
    ?INT_DBG("clear log: Msg = ~p, Flows = ~p", [Msg, Flows]),
    Res = case RuleId of
            '' when is_list(Flows) ->
                OutputList = lists:usort(lists:foldl(fun ({_, L}, Acc) -> L ++ Acc end, [], Flows)),
                case lists:foldl(
                        fun (#flow_handle{id = Handle}, Acc) ->
                            case chronica_gen_backend:clear(Handle) of
                                ok -> Acc;
                                Err -> [{failed, {Handle, Err}} | Acc]
                            end
                        end, [], OutputList) of
                    [] ->
                        ok;
                    Err2 -> Err2
                end;
            _ when is_list(Flows) ->
                case lists:keyfind(RuleId, 1, Flows) of
                    false -> {error, not_found};
                    {_, Outputs} ->
                        case lists:foldl(
                                fun (#flow_handle{id = Handle}, Acc) ->
                                    case chronica_gen_backend:clear(Handle) of
                                        ok -> Acc;
                                        Err -> [{failed, {Handle, Err}} | Acc]
                                    end
                                end, [], lists:usort(Outputs)) of
                            [] -> ok;
                            Err2 -> Err2
                        end
                end;
            _ ->
                ok
        end,
    {reply, Res, State};

handle_call(get_config, _From, State = #config_state{loaded_config = Config}) ->
    {reply, Config, State};

handle_call({update_rule_inwork, IdList, InWork, TickFun}, _From,
            State = #config_state{loaded_config = Config =
                                  #chronica_config{rules = Rules}})
  when is_list(IdList) ->
    Fun = fun(OneId, Acc) ->
                  case lists:keyfind(OneId, #chronica_rule.id, Acc) of
                      false ->
                          Acc;
                      #chronica_rule{in_work = InWork} ->
                          Acc;
                      Rule ->
                          lists:keyreplace(OneId, #chronica_rule.id, Acc, Rule#chronica_rule{in_work = InWork})
                  end
          end,
    NewRules = lists:foldl(Fun, Rules, IdList),
    case catch true_load_config(State, Config#chronica_config{rules = NewRules}, TickFun) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, NewState, Reason} -> {reply, {error, Reason}, NewState};
        {stop, Reason} -> {stop, Reason, {error, Reason}, State};
        Err -> {stop, Err, State}
    end;
handle_call({update_rule_inwork, Id, InWork, TickFun}, _From,
            State = #config_state{loaded_config = Config = #chronica_config{rules = Rules}}) ->
    case lists:keyfind(Id, #chronica_rule.id, Rules) of
        false -> {reply, {error, not_found}, State};
        #chronica_rule{in_work = InWork} -> {reply, {error, already_set}, State};
        Rule ->
            NewRules = lists:keyreplace(Id, #chronica_rule.id, Rules, Rule#chronica_rule{in_work = InWork}),
            case catch true_load_config(State, Config#chronica_config{rules = NewRules}, TickFun) of
                {ok, NewState} -> {reply, ok, NewState};
                {error, NewState, Reason} -> {reply, {error, Reason}, NewState};
                {stop, Reason} -> {stop, Reason, {error, Reason}, State};
                Err -> {stop, Err, State}
            end
    end;

handle_call({add_rule, NameRule, Mask, Priority, Flow, TickFun}, _From,
            State = #config_state{loaded_config = Config = #chronica_config{rules = Rules}}) ->
    NewRule = #chronica_rule{id = NameRule, mask = Mask,
                priority = Priority, flow_ids = [Flow], in_work = true},
    case catch true_load_config(State, Config#chronica_config{rules = [NewRule|Rules]}, TickFun) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, NewState, Reason} -> {reply, {error, Reason}, NewState};
        {stop, Reason} -> {stop, Reason, {error, Reason}, State};
        Err -> {stop, Err, State}
    end;

handle_call(get_module_num, _From, State = #config_state{registered_applications = RegApps}) ->
    {reply, sets:size(RegApps), State};

handle_call(get_flow_names, _From, State = #config_state{loaded_config = #chronica_config{flows = Flows}}) ->
    Res = lists:foldl(
        fun (#chronica_flow{flow_id = Name}, Acc) ->
            [Name | Acc]
        end, [], Flows),
    {reply, Res, State};

handle_call(get_output_list, _From, State = #config_state{rules = Rules}) ->
    try
        Res =
            case Rules of
                null -> [];
                _ -> get_all_flows(Rules)
            end,
        {reply, {ok, Res}, State}
    catch
        C:E ->
            ?INT_EXCEPT("Exception: ~p:~p", [C, E]),
            {reply, {error, E}, State}
    end;

handle_call(free_resources, _From, State = #config_state{rules = Rules}) ->
    ?INT_DBG("free_resources received", []),
    try
        case Rules of
            null -> ok;
            _ ->
                FlowHandles = get_all_flows(Rules),
                close_outputs(FlowHandles, ?wait_before_close_output_at_stop_timeout)
        end
    catch
        C:E -> ?INT_EXCEPT("Exception: ~p:~p", [C, E])
    end,
    {reply, ok, State};

handle_call(rotate, _From, State = #config_state{rules = Rules}) ->
    ?INT_DBG("rotate received", []),
    Res =
        try
            case Rules of
                null -> throw(not_loaded);
                _ -> ok
            end,

            Flows = get_all_flows(Rules),

            lists:foreach(
                fun (Handle) ->
                    case chronica_gen_backend:rotate(Handle) of
                        ok -> ok;
                        Err -> throw(Err)
                    end
                end,
                Flows)
        catch
            throw:not_loaded -> ok;
            throw:E -> E
        end,
    {reply, Res, State};

handle_call({add_application, App}, _From, State =
            #config_state{registered_applications = RApps, rules = Rules,
                          loaded_config = #chronica_config{detail_info = Detail_info},
                          cache = Cache, cache_timer = IsCacheTimerStarted}) ->
    ?INT_DBG("add_application received ~p", [App]),
    {Res, {NewRApps, NewCache}} = add_application(App, RApps, Rules, Cache, Detail_info),
    NewIsCacheTimerStarted =
        case NewCache == Cache of
            true  -> IsCacheTimerStarted;
            false -> IsCacheTimerStarted orelse timer:send_after(?save_cache_timeout, save_cache), true
        end,
    {reply, Res, State#config_state{registered_applications = NewRApps,
                        cache = NewCache, cache_timer = NewIsCacheTimerStarted}};

handle_call({test_add_module, Module}, _From, #config_state{rules = Rules} = State) ->
    ?INT_DBG("test_add_module received ~p", [Module]),
    Code = chronica_iface:generate_iface_module(Module, Rules),
    catch load_app_iface_modules([{Module, Code}]),
    {reply, ok, State};

handle_call(print_state, _From, State) ->
    io:format("manager's state:~n~p~n", [State]),
    {reply, ok, State};

handle_call({active, NewActive}, _From, State = #config_state{loaded_config = Config}) ->
    case catch true_load_config(State, Config#chronica_config{active = NewActive}) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, NewState, Reason} -> {reply, {error, Reason}, NewState};
        {stop, Reason} -> {stop, Reason, {error, Reason}, State};
        Err -> {stop, Err, {error, Err}, State}
    end;

handle_call(_Msg, _From, State) ->
    ?INT_ERR("Unhandled call request ~p", [_Msg]),
    {reply, {error, bad_msg}, State}.

handle_info({'EXIT', Pid, normal}, State) ->
    ?INT_DBG("EXIT received from ~p with reason normal", [Pid]),
    {noreply, State};

handle_info(check_log_backend, #config_state{flows = RawFlows, loaded_config = #chronica_config{}} = State) ->
    Flows = lists:flatten([LFlows || {_, LFlows} <- RawFlows]),
    CheckFun =
        fun(#flow_handle{id = Handle, output_module = OutputModule, open_params = OpenParam, writer_options = WriterOptions} = LFlow) ->
            case chronica_gen_backend:check(Handle) of
                true ->
                    ok;
                _ ->
                    ?INT_WARN("Chronica backend ~p was closed. Try to reopening...", [Handle]),
                    case chronica_gen_backend:open(OutputModule, OpenParam, WriterOptions) of
                        {ok, Handle} ->
                            ?INT_WARN("Chronica backend ~p was reopend.", [Handle]),
                            ok;
                        {error, Error} ->
                            ?INT_WARN("Error open output: ~p Reason: ~p", [LFlow, Error]),
                            ok
                    end
            end
        end,
    lists:foreach(CheckFun, Flows),
    timer:send_after(?check_log_backend_timeout, check_log_backend),
    {noreply, State};

handle_info(save_cache, State = #config_state{config_hash = undefined}) ->
    {noreply, State};
handle_info(save_cache, State = #config_state{loaded_config =
        #chronica_config{data_root = CacheDir}, cache = Cache, config_hash = ConfigHash}) ->
    ?INT_DBG("Save cache timer expired", []),
    chronica_cache:save(CacheDir, ConfigHash, Cache),
    {noreply, State#config_state{cache_timer = false}};

handle_info(_Info, State) ->
    ?INT_ERR("Unhandled info ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    ?INT_DBG("terminate: Reason = ~p", [_Reason]),
    handle_call(free_resources, self(), State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start(State, Config) ->
    try
        chronica_config:validate(Config)
    catch
        throw:Error -> ?INT_ERR("Config is not valid: ~p", [Error]), throw({bad_config, Error})
    end,

    case load_config(State, Config) of
        {ok, NewNewState} -> {ok, NewNewState};
        Err -> Err
    end.


unload_config(S = #config_state{loaded_config = #chronica_config{}, flows = Flows}) when Flows =/= null ->
    case catch destroy_flows(Flows, ?wait_before_close_output_at_cfg_reload_timeout) of
        ok -> S#config_state{loaded_config = #chronica_config{}, flows = null};
        E ->
            ?INT_ERR("Error while processing destoy flows: ~p", [E]),
            S
    end;
unload_config(S) ->
    S.


true_load_config(S, NewConfig) ->
    true_load_config(S, NewConfig, undefined).

% TickFun is used to notify parent about events,
% for example: it is needed to support cocon progress bars

true_load_config(S = #config_state{loaded_config = OldConfig}, OldConfig, _) -> {ok, S};
true_load_config(S = #config_state{loaded_config = OldConfig}, NewConfig, TickFun) ->

    Res =
        try
            chronica_config_validation:validate(NewConfig),
            NewState = unload_config(S),
            case load_config(NewState, NewConfig, TickFun) of
                {ok, _} = RR -> RR;
                LoadError ->
                    ?INT_ERR("Load config error: ~p, falling back to old config...", [LoadError]),
                    case load_config(NewState, OldConfig) of
                        {ok, NewNewState} ->
                            {error, NewNewState, LoadError};
                        LoadError2 ->
                            ?INT_ERR("Load old config error: ~p, gonna crash...", [LoadError2]),
                            {stop, {LoadError, LoadError2}}
                    end
            end
        catch
            throw:Err -> {error, S, Err}
        end,
    Res.

load_config(State, NewConfig) -> load_config(State, NewConfig, undefined).

load_config(State = #config_state{loaded_config = LoadedConfig = #chronica_config{active = LoadedActive, formats = OldFormatsConfig}, registered_applications = RegApps, cache_timer = IsCacheTimerStarted},
            NewConfig = #chronica_config{data_root = CacheDir},
            TickFun) ->
    ?INT_DBG("load rules: NewConfig = ~p", [NewConfig]),
    try
        case LoadedConfig of
            NewConfig ->
                throw({ok, State});
            _ -> ok
        end,

        #chronica_config{
                    rules = NewRulesConfig,
                    flows = NewFlowsConfig,
                    formats = NewFormatsConfig,
                    colors = NewColorsConfig,
                    data_root = DataRoot,
                    log_root = LogRoot,
                    max_file_size = NewMaxFileSize,
                    max_file_num = NewMaxFileNum,
                    rotate_at_start = NewRotateStartup,
                    active = Active,
                    detail_info = Detail_info,
                    tty_enabled = TTYEnabled,
                    backend_modules = BackendModules
                } = NewConfig,

        case Active of
            true -> switch_on();
            false ->
                switch_off(),
                ConfigHashForOff = config_hash(NewConfig),
                CacheForOff = chronica_cache:load(CacheDir, ConfigHashForOff),
                throw({ok, State#config_state{
                            loaded_config = NewConfig,
                            rules = [],
                            flows = [],
                            cache = CacheForOff,
                            config_hash = ConfigHashForOff
                        }})
        end,

        case NewFormatsConfig of
            OldFormatsConfig when LoadedActive =:= true -> ok;
            _ ->
                FunctionList = parse_formats(NewFormatsConfig, [], NewColorsConfig),
                case chronica_format_creator:reload_formats(FunctionList, chronica_format) of
                    ok -> ok;
                    Error ->
                        ?INT_ERR("cant load formats cause format_creator error: ~p", [Error]),
                        throw({load_error, Error})
                end
        end,

        % Do not add them to head!
        BackendModules2 = BackendModules ++ [{tty, chronica_tty_backend},
                                             {udp, chronica_udp_backend},
                                             {tcp_con, chronica_tcp_con_backend},
                                             {file, chronica_disk_log_backend}],

        WriterOptions = [{log_root, LogRoot}, {data_root, DataRoot}, {max_file_size, NewMaxFileSize}, {max_file_num, NewMaxFileNum}, {tty_enabled, TTYEnabled}],

        NewFlows = create_flows(NewFlowsConfig, WriterOptions, BackendModules2),
        case NewRotateStartup of
            true ->
                lists:foreach(
                    fun (WHandle) ->
                        catch chronica_gen_backend:rotate(WHandle)
                    end,
                    get_writers(NewFlows));
            false -> ok
        end,
        ClearFlowHandleFun =
            fun({LName, LFlows}) ->
                {LName, [LFlow#flow_handle{open_params = undefined, output_module = undefined, writer_options = undefined} || #flow_handle{} = LFlow <- LFlows]}
            end,
        ResFlows = lists:map(ClearFlowHandleFun, NewFlows),
        NewRules = chronica_config:parse_rules(NewRulesConfig, ResFlows),
        ConfigHash = config_hash(NewConfig),
        Cache = chronica_cache:load(CacheDir, ConfigHash),

        ?INT_DBG("ADD ALL APPLICATIONS", []),
        {NewRegApps, NewCache} = add_all_applications(RegApps, NewRules, Cache, Detail_info, TickFun),

        erlang:garbage_collect(),

        NewIsCacheTimerStarted =
            case NewCache == Cache of
                true ->
                    IsCacheTimerStarted;
                false ->
                    IsCacheTimerStarted orelse timer:send_after(?save_cache_timeout, save_cache),
                    true
            end,

        {ok, State#config_state{
                loaded_config = NewConfig,
                rules = NewRules,
                flows = NewFlows,
                registered_applications = NewRegApps,
                cache = NewCache,
                cache_timer = NewIsCacheTimerStarted,
                config_hash = ConfigHash
                }}
    catch
        throw:{load_error, Reason} -> {error, Reason};
        throw:{bad_config, _} = Err2 -> Err2;
        throw:{ok, S} -> {ok, S};
        throw:E -> {error, E};
        _:E -> {error, {E, erlang:get_stacktrace()}}
    end.

get_all_flows(Rules) ->
    FlowHandlesDublicated = [ Handle || {_Format, Handle}  <-
        lists:foldl(
          fun (#rule{flows = Flows}, Acc) -> Flows ++ Acc end, [], Rules)
        ],
    lists:usort(FlowHandlesDublicated).

get_config() ->
    gen_server:call(?MODULE, get_config).

update_rule_inwork(Id, InWork) ->
    update_rule_inwork(Id, InWork, undefined).

update_rule_inwork(Id, InWork, TickFun) ->
    gen_server:call(?MODULE, {update_rule_inwork, Id, InWork, TickFun}, infinity).

get_module_num() ->
    gen_server:call(?MODULE, get_module_num, infinity).

load_config(Config) ->
    gen_server:call(?MODULE, {load_config, Config}, infinity).
clear_log(RuleId) ->
    gen_server:call(?MODULE, {clear_log, RuleId}).
active(Param) ->
    gen_server:call(?MODULE, {active, Param}, infinity).
rotate() ->
    gen_server:call(?MODULE, rotate).
free_resources() ->
    gen_server:call(?MODULE, free_resources).
add_application(App) ->
    gen_server:call(?MODULE, {add_application, App}, infinity).

-spec add_rule(NameRule :: atom(), Mask :: nonempty_string(),
    Priority :: chronica_priority(), Flow :: atom()) -> ok | {error, _Reason}.
add_rule(NameRule, Mask, Priority, Flow) when is_atom(NameRule) ->
    add_rule(NameRule, Mask, Priority, Flow, fun() -> ok end).

-spec add_rule(NameRule :: atom(), Mask :: nonempty_string(),
    Priority :: chronica_priority(), Flow :: atom(), Fun :: fun(() -> any())) -> ok | {error, _Reason}.
add_rule(NameRule, Mask, Priority, Flow, Fun) when is_atom(NameRule) ->
    gen_server:call(?MODULE, {add_rule, NameRule, Mask, Priority, Flow, Fun}, infinity).

test_add_module(Module) ->
    gen_server:call(?MODULE, {test_add_module, Module}).

close_outputs([Handle | Tail], Timeout) ->
    ?INT_DBG("Close of output ~p cause termination", [Handle]),
    chronica_gen_backend:close(Handle, Timeout),
    close_outputs(Tail, Timeout);
close_outputs([], _Timeout) ->
    ok.

switch_on() ->
    ?INT_DBG("switch_on", []),
    Res = pt_recompilable:recompile_orig_module(chronica_core,
        fun (AST, _Options) ->
            AST
        end
    ),
    Res.

switch_off() ->
    ?INT_DBG("switch_off", []),
    Res = pt_recompilable:recompile_orig_module(chronica_core,
        fun (AST, _Options) ->
                pt_lib:replace(AST, ast_pattern("log_fast(...$P...) -> ...$_... .", Line), ast("log_fast(...$P...) -> ok. ", Line))
        end),
    Res.

create_flows(ConfigFlows, WriterOptions, Backends) ->
    FlowsList = chronica_config:parse_flows(ConfigFlows, WriterOptions, Backends),
    {_, Doubling} = lists:foldl(
                        fun ({Name, _}, {S , D}) ->
                            case lists:member(Name, S) of
                                true -> {S, [Name | D]};
                                false -> {[Name | S], D}
                            end
                        end,
                        {[], []},
                        FlowsList),
    case Doubling of
        [] -> ok;
        _ ->
            ?INT_ERR("following flows are doubling: ~p", [Doubling]),
            erlang:throw({create_flows, doubling_flows})
    end,
    FlowsList.

destroy_flows(Flows, Timeout) ->
    Writers = get_writers(Flows),
    close_outputs(Writers, Timeout).

get_writers(null) -> [];
get_writers(Flows) ->
    lists:foldl(
        fun ({_Name, Outputs}, Acc) ->
            [Handle || #flow_handle{id = Handle} <- Outputs] ++ Acc
        end,
        [],
        Flows).

parse_formats([], Res, _) -> Res;
parse_formats([#chronica_format{format_id = Name, format = UserFormat} | Tail], Res, ColorsConfig) when is_atom(Name) and is_list(UserFormat) ->
    parse_formats(Tail, [chronica_format_creator:create_format_function(Name, UserFormat, ColorsConfig) | Res], ColorsConfig);
parse_formats(BadFormated, _, _) ->
    ?INT_ERR("invalid formats param ~p", [BadFormated]),
    erlang:throw({parse_formats, bad_format}).

add_application(App, RApps, Rules, Cache, Detail_info) ->
    add_application(App, RApps, Rules, Cache, Detail_info, undefined).

add_application(App, RApps, Rules, Cache, Detail_info, TickFun) ->
    try
        add_application_(App, RApps, Rules, Cache, Detail_info, TickFun)
    catch
        throw:{Err, NRA} ->
            ?INT_ERR("reg ~p failed \\ ~p~n", [App, NRA]),
            {Err, NRA}
    end.

add_application_(App, RApps, Rules, Cache, Detail_info, TickFun) ->
    ?INT_DBG("-> rq add_application ~p \\ ~p", [App, RApps]),
    case sets:is_element(App, RApps) of
        true ->
            {ok, {RApps, Cache}};
        false ->
            ?INT_DBG("-> Rq Reg ~100000p \\ ~100000p \\ is_list(Cache): ~10000000000000p~n", [App, RApps, is_list(Cache)]),
            case add_application_in_cache(App, RApps, Rules, Cache, Detail_info, TickFun) of
                % Applicatin successfully added in cache.
                {ok, {NewRApps, NewCache}} ->
                    DepApps0 = case application:get_key(App, applications) of
                                   {ok, L0} -> L0;
                                   _ -> []
                               end,
                    DepApps = case application:get_key(App, included_applications) of
                                  {ok, [_ | _] = L1} -> L1 ++ DepApps0;
                                  _ -> DepApps0
                              end,
                    AddAppsRecurciveFun =
                    fun (A, {Added, CacheAcc}) ->
                            case add_application_(A, Added, Rules, CacheAcc, Detail_info, TickFun) of
                                {ok, {NewAdded, NewCacheAcc}} -> {NewAdded, NewCacheAcc};
                                {Error, {NewAdded, NewCacheAcc}} -> throw({Error, {NewAdded, NewCacheAcc}})
                            end
                    end,
                    {ResultRApps, ResultCache} = lists:foldl(AddAppsRecurciveFun, {NewRApps, NewCache}, DepApps),
                    {ok, {ResultRApps, ResultCache}};
                % Error during add application in cache.
                {Error, ErrorArgs} ->
                    {Error, ErrorArgs}
            end
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
add_application_in_cache(App, RApps, Rules, Cache, Detail_info, TickFun) ->
    ?INT_DBG("Registering application ~100000p \\ ~1000000p \\ is_list(NewCache): ~10000000p", [App, RApps, is_list(Cache)]),
    try
        AppHash = app_hash(App),
        is_function(TickFun) andalso TickFun(),
        case proplists:get_value(App, Cache, undefined) of
            % do not recompile, use cache
            {AppHash, AppModulesCode} ->
                ?INT_DBG("~p chronica interface was loaded from cache", [App]),
                load_app_iface_modules(AppModulesCode),
                ResApps = sets:add_element(App, RApps),
                ?INT_DBG("Registered (from cache) \\ ~p~n", [ResApps]),
                {ok, {ResApps, Cache}};
            % no cache or old cache
            _Cached ->
                AppModulesCode = case Detail_info of
                    true ->
                        io:format("Compiling the cache: [~p] ", [App]),
                        T1 = erlang:system_time(millisecond),
                        Apps = chronica_iface:generate_app_iface_modules(App, Rules),
                        T2 = erlang:system_time(millisecond),
                        case Apps of
                            [] -> io:format("skipped~n");
                            _ -> io:format("~pms done.~n", [T2 - T1])
                        end,
                        Apps;
                    false ->
                        chronica_iface:generate_app_iface_modules(App, Rules)
                end,
                load_app_iface_modules(AppModulesCode),
                ResApps = sets:add_element(App, RApps),
                ?INT_DBG("Registered \\ ~p~n", [ResApps]),
                {ok, {ResApps, [{App, {AppHash, AppModulesCode}} | proplists:delete(App, Cache)]}}
        end
    catch
        _:Error ->
            ?INT_ERR("Add application ~p failed cause: ~p~n~p", [App, Error, erlang:get_stacktrace()]),
            {Error, {RApps, Cache}}
    end.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set_configured_true() ->
    pt_recompilable:recompile_cur_module(
        chronica_status,
        fun (AST, _Options) ->
            pt_lib:replace(AST,
                ast_pattern("configured() -> ...$_... .", Line),
                ast("configured() -> true.", Line))
        end).

depended_from_chronica(lager) ->
    true;
depended_from_chronica(chronica) ->
    true;
depended_from_chronica(App) ->
    CheckApplicationFun =
        fun({ok, DependedApplications}) ->
                lists:member(chronica, DependedApplications) orelse
                    lists:member(lager, DependedApplications);
           (_) ->
                false
        end,
    CheckApplicationFun(application:get_key(App, applications)) orelse
        CheckApplicationFun(application:get_key(App, included_applications)).

app_hash(App) ->
    Tags =
        case depended_from_chronica(App) of
            false ->
                [];
            true ->
                case application:get_key(App, modules) of
                    {ok, L2} ->
                        lists:foldl(
                          fun (ModuleName, Acc) ->
                                  case (catch ModuleName:get_log_tags()) of
                                      Tags2 when is_list(Tags2) -> [{ModuleName, Tags2}|Acc];
                                      _ -> Acc
                                  end
                          end, [], L2);
                    Error -> erlang:error({no_modules_info, App, Error})
                end
        end,
    erlang:md5(erlang:term_to_binary(Tags)).

config_hash(#chronica_config{active = Active, rules = Rules, flows = Flows}) ->
    erlang:md5(erlang:term_to_binary({Active, Rules, Flows})).

add_all_applications(RegApps, NewRules, Cache, Detail_info, TickFun) ->
    sets:fold(
        fun (App, {RAAcc, CacheAcc}) ->
            case add_application(App, RAAcc, NewRules, CacheAcc, Detail_info, TickFun) of
                {ok, NRA} -> NRA;
                {AAErr, _NRA} -> throw(AAErr)
            end
        end,
        {sets:new(), Cache},
        RegApps).

load_code_module(undefined) -> ok;
load_code_module({Module, Binary}) ->
    case code:soft_purge(Module) of
        true ->
            case code:load_binary(Module, atom_to_list(Module) ++ ".erl", Binary) of
                {module, Module} ->
                    ?INT_DBG("~p: module reloaded", [Module]),
                    ok;
                {error, Cause} -> {error, {cant_load, Module, Cause}}
            end;
        false ->
            {error, soft_purge_failed}
    end.

load_app_iface_modules(AppModulesCode) ->
    lists:foreach(
        fun ({Module, Code}) ->
            case load_code_module(Code) of
                ok ->
                    ok;
                {error, Error} ->
                    ?INT_ERR("Cant load module ~p chronica iface code binary, reason: ~p", [Module, Error]),
                    erlang:error({cant_load_module, Module, Error})
            end
        end, AppModulesCode).



-define(d2012_11_16_1_25_41, 63520248341000000).

generate_name(Prefix) ->
    Now = {_, _, M} = os:timestamp(),
    DT = calendar:now_to_universal_time(Now),
    N = calendar:datetime_to_gregorian_seconds(DT) * 1000000 + M - ?d2012_11_16_1_25_41,

    lists:flatten(io_lib:format("~s~.36.0b", [Prefix, N])).

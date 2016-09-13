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
    read_config/1,
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


-type rule() :: string() | {string(), rule()} | {string(), rule(), rule()}.
-record(rule,
    {
        id = 0 :: integer(),
        rule_str = "*"   :: rule(),
        log_priority = 0 :: integer(),
        flows :: [term()]
    }).

-record(config_state,
    {
        loaded_config :: #chronica_config{},
        rules :: [#rule{}] | null,
        flows :: [term()] | null,
        create_time :: {integer(), integer(), integer()},
        registered_applications = [chronica] :: [atom()],
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
                create_time = Now
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
        {value, _, NewFlows} ->
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
    Cache = load_cache(CacheDir, ConfigHash),

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
            Res = get_appropriate_flows(lists:usort(Tags), Priority, Rules),
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
    {reply, erlang:length(RegApps), State};

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
    Code = generate_iface_module(Module, Rules),
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
    save_cache(CacheDir, ConfigHash, Cache),
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
        chronica_config_validation:validate(Config)
    catch
        throw:Error -> ?INT_ERR("Config is not valid: ~p", [Error]), throw({bad_config, Error})
    end,

    case load_config(State, Config) of
        {ok, NewNewState} -> {ok, NewNewState};
        Err -> Err
    end.

-spec default_env(Property :: atom()) -> term().
default_env(active)          -> true;
default_env(backend_modules) -> [];
default_env(colored)         -> false;
default_env(data_root)       -> "./cache_<Node>/";
default_env(detail_info)     -> false;
default_env(log_root)        -> "./log_<Node>/";
default_env(max_file_num)    -> 10;
default_env(max_file_size)   -> 10 * 1024 * 1024;
default_env(rotate_at_start) -> false;
default_env(tcp_host)        -> any;
default_env(tcp_port)        -> 0;
default_env(tty_enabled)     -> true;
default_env(flows) ->
    [
     {file_info_log,   [{file, "console.log"}]},
     {error_log,       [{file, "error.log"}]},
     {screen_info_log, [{tty, short}]}
    ];
default_env(formats) ->
    [
     {short, "%H:%Mi:%S.%Ms [%Priority] %Message\n"}
    ];
default_env(internal_logger) ->
    [
     {file, "chronica", {101048576, 1}, info},
     {tty, error}
    ];
default_env(rules) ->
    [
     {file_info_log,   "*", info,  [file_info_log], on},
     {error_log,       "*", error, [error_log], on},
     {screen_info_log, "*", info,  [screen_info_log], on}
    ];
default_env(V) ->
    throw({not_found, V}).

-spec read_config(Now :: term()) -> {ok, #chronica_config{}} | {error, term()}.
read_config(Now) ->
    try
        Get = fun (V) ->
            ?INT_DBG("get_app_env: ~p~n", [V]),
            application:get_env(chronica, V, default_env(V))
        end,

        % for colored tty
        ConfigColors = read_coloring(Get(colored)),
        ConfigRules = read_rules(Get(rules), [], 1),
        ConfigFlows = read_flows(Get(flows), []),
        ConfigFormats = read_formats(Get(formats), []),
        {NewConfigColors, NewConfigFlows, NewConfigFormats} =
        case ConfigColors#chronica_coloring.colored of
            false ->
                {ConfigColors, ConfigFlows, ConfigFormats};
            true ->
                EndLineFormatsName = ConfigColors#chronica_coloring.end_line_formats_name,
                WrapEndLineFormatsName = wrap_end_line_formats_name_colored(max_len_formats(ConfigFormats, 0), EndLineFormatsName),
                NamesFormatColor = remove_duplicate(read_flows_tty(Get(flows), [], WrapEndLineFormatsName), []),
                {
                    ConfigColors#chronica_coloring{formats_name = NamesFormatColor},
                    add_flows_color(ConfigFlows, [], WrapEndLineFormatsName),
                    add_formats_color(ConfigFormats, NamesFormatColor, [])
                }
        end,

        LogRootDir = get_log_root(Get(log_root), Now),
        DataRootDir = get_data_root(Get(data_root)),

        ILParams = case Get(internal_logger) of
            ILP when is_list(ILP) ->
                lists:map(
                    fun ({file, FName, ILSizes, ILFilter}) ->
                        {file, LogRootDir ++ "/" ++ FName, ILSizes, ILFilter};
                        (OP) -> OP
                    end,
                    ILP);
            ILP -> throw({error, {bad_internal_logger, ILP}})
        end,

        FileAttr = {Get(max_file_size), Get(max_file_num)},
        ErrLog = fun() ->
                    io:format("Bad chronica config: Using old-style "
                            "internal_logger params, update it!~n", [])
                    end,
        ILParams2 = case ILParams of
            [file] ->
                FileFlow = LogRootDir ++ "/" ++ Get(internal_logger_filename),
                ErrLog(),
                [{tty, warning}, {FileFlow, FileAttr, warning}];
            [debug] ->
                ErrLog(),
                [{tty, debug}];
            [file, debug] ->
                FileFlow = LogRootDir ++ "/" ++ Get(internal_logger_filename),
                ErrLog(),
                [{tty, debug}, {file, FileFlow, FileAttr, debug}];
            [debug, file] ->
                FileFlow = LogRootDir ++ "/" ++ Get(internal_logger_filename),
                ErrLog(),
                [{tty, debug}, {file, FileFlow, FileAttr, debug}];
            Other -> Other
        end,

        Config = #chronica_config{
                    rules = ConfigRules,
                    flows = NewConfigFlows,
                    formats = NewConfigFormats,
                    colors = NewConfigColors,
                    active = Get('active'),
                    detail_info = Get('detail_info'),
                    rotate_at_start = Get(rotate_at_start),
                    internal_logger = ILParams2,
                    data_root = DataRootDir,
                    log_root = LogRootDir,
                    max_file_size = Get(max_file_size),
                    max_file_num = Get(max_file_num),
                    tty_enabled = Get(tty_enabled),
                    tcp_port = Get(tcp_port),
                    tcp_host = Get(tcp_host),
                    backend_modules = Get(backend_modules)
                   },
        {ok, Config}
    catch
        throw:E -> {error, E};
        _:E -> {error, {E, erlang:get_stacktrace()}}
    end.

read_rules([], Res, _) -> lists:reverse(Res);

read_rules([{RuleName, Mask, Priority, FList, InWork} = RR | Tail], Res, N) ->

    InWorkValue =
        case InWork of
            on  -> true;
            off -> false
        end,

    FList2 =
        case is_list(FList) of
            false ->
                io:format("WARNING: invalid chronica config: Flow list should be a list (rule: ~1000000p)~n", [RR]),
                [FList];
            true -> FList
        end,

    Mask2 =
        case is_atom(Mask) of
            true  ->
                io:format("WARNING: invalid chronica config: Mask should be a string (rule: ~1000000p)~n", [RR]),
                erlang:atom_to_list(Mask);
            false -> Mask
        end,


    {Mask3, RuleId} = case re:run(Mask2, "^(?<RULE>\\S+)\\/(?<ID>\\S+)$", [{capture, ['RULE', 'ID'], list}]) of
                        {match, [S, ID]} -> {S, ID};
                        nomatch -> {Mask2, ""}
                       end,

    case RuleId of
        "" -> ok;
        _  -> io:format("WARNING: invalid chronica config: Old mask format - remove \"/*\" tail (rule: ~1000000p)~n", [RR])
    end,

    {RuleName2, N2} =
        case RuleName of
            undefined -> mk_rule_name("rule", N, Res);
            ''        -> mk_rule_name("rule", N, Res);
            ""        -> mk_rule_name("rule", N, Res);
            _ when is_list(RuleName) -> mk_rule_name(RuleName, N, Res);
            _ when is_atom(RuleName) -> mk_rule_name(erlang:atom_to_list(RuleName), N, Res)
        end,

    read_rules(Tail, [#chronica_rule{id = RuleName2, mask = Mask3, priority = Priority, flow_ids = FList2, in_work = InWorkValue} | Res], N2);

read_rules([{Mask, Priority, FList, InWork} = RR | Tail], Res, N) ->
    io:format("WARNING: invalid chronica config: obsolete rule format, actual: {RuleName, Mask, Priority, Flows, InWork} (rule: ~1000000p)~n", [RR]),
    read_rules([{undefined, Mask, Priority, FList, InWork} | Tail], Res, N);


read_rules([{Mask, Priority, FList} = RR | Tail], Res, N)  ->
    io:format("WARNING: invalid chronica config: obsolete rule format, actual: {RuleName, Mask, Priority, Flows, InWork} (rule: ~1000000p)~n", [RR]),
    read_rules([{undefined, Mask, Priority, FList, on} | Tail], Res, N).

mk_rule_name(Str, N, Rules) ->
    Id = erlang:list_to_atom(Str),
    case lists:keyfind(Id, #chronica_rule.id, Rules) of
        false -> {Id, N};
        _ ->
            Id2 = erlang:list_to_atom(Str ++ erlang:integer_to_list(N)),

            case lists:keyfind(Id2, #chronica_rule.id, Rules) of
                false -> {Id2, N + 1};
                _     -> mk_rule_name(Str, N + 1, Rules)
            end
    end.

add_formats_color([#chronica_format{format_id = FormatId, format = Format} = Head| Tail], NamesFormatColor, Acc) ->
    case lists:keyfind(FormatId, 1, NamesFormatColor) of
        {FormatId, NameFormatColor} ->
            NewAcc = [Head, #chronica_format{format_id = NameFormatColor, format = Format}| Acc],
            add_formats_color(Tail, NamesFormatColor, NewAcc);
        false ->
            add_formats_color(Tail, NamesFormatColor, [Head| Acc])
    end;
add_formats_color([], _, Acc) -> Acc.

%for end_line_formats_name
wrap_end_line_formats_name_colored(Len, EndLineFormatsName) ->
    lists:concat(lists:duplicate(Len, "_")) ++ EndLineFormatsName.

max_len_formats([], Len) ->
    LenDefault = string:len("default"),
    if
        LenDefault < Len -> Len;
        true             -> LenDefault
    end;
max_len_formats([#chronica_format{format_id = FormatId} | Tail], Len) ->
    LenFormatId = string:len(atom_to_list(FormatId)),
    NewLen =
        if
            Len < LenFormatId -> LenFormatId;
            true              -> Len
        end,
    max_len_formats(Tail, NewLen).

%add color format to flow
add_tty_color(#chronica_backend{type = Type, format = Format} = Tty, EndLineFormatsName) ->
    case Type of
        {tty, undefined} ->
            Tty#chronica_backend{format = list_to_atom(lists:concat([Format, EndLineFormatsName]))};
        _ -> Tty
    end.
add_flow_color([Head| Tail], Acc, EndLineFormatsName) ->
    add_flow_color(Tail, [add_tty_color(Head, EndLineFormatsName)| Acc], EndLineFormatsName);
add_flow_color([], Acc, _EndLineFormatsName) -> Acc.

%add color formats to flows
add_flows_color([#chronica_flow{backends = Backends} = Head| Tail], Acc, EndLineFormatsName) ->
    NewBackends = add_flow_color(Backends, [], EndLineFormatsName),
    add_flows_color(Tail, [Head#chronica_flow{backends = NewBackends}| Acc], EndLineFormatsName);
add_flows_color([], Acc, _EndLineFormatsName) -> Acc.

%remove duplicate formats
remove_duplicate([Head| Tail], Acc) ->
    remove_duplicate([Body || Body <- Tail, Body =/= Head, Body =/= []], [Head| Acc]);
remove_duplicate([], Acc) -> Acc.


read_flows_tty([], Res, _EndLineFormatsName) -> Res;
read_flows_tty([{FlowId, List}| Tail], Res, EndLineFormatsName) when is_atom(FlowId), is_list(List) ->
    case [X || X <- read_writers_tty(List, []), X =/= []] of %list may be empty
        []      -> read_flows_tty(Tail, Res, EndLineFormatsName);
        [Other] -> read_flows_tty(Tail, [{Other, list_to_atom(lists:concat([Other, EndLineFormatsName]))}| Res], EndLineFormatsName)
    end.

read_writers_tty([], Res) -> Res;
read_writers_tty([tty | Tail], Res) ->
    read_writers_tty(Tail, [default | Res]);
read_writers_tty([{tty, Format} | Tail], Res) ->
    read_writers_tty(Tail, [Format | Res]);
read_writers_tty([{_Type, _Params} | Tail], Res) ->
    read_writers_tty(Tail, Res);
read_writers_tty([{_Type, _Params, _Format} | Tail], Res) ->
    read_writers_tty(Tail, Res);
read_writers_tty([W | _Tail], _Res) ->
    throw({bad_config, {bad_writer_tty, W}}).

read_flows([], Res) -> Res;
read_flows([{FlowId, List} | Tail], Res) when is_atom(FlowId), is_list(List) ->
    Writers = read_writers(List, []),
    read_flows(Tail, [#chronica_flow{flow_id = FlowId, backends = Writers} | Res]).

read_writers([], Res) -> Res;
read_writers([tty | Tail], Res) ->
    read_writers(Tail, [#chronica_backend{type = {tty, undefined}, format = default} | Res]);
read_writers([{tty, Format} | Tail], Res) ->
    read_writers(Tail, [#chronica_backend{type = {tty, undefined}, format = Format} | Res]);
read_writers([{Type, Params}|Tail], Res) ->
    read_writers(Tail, [#chronica_backend{type = {Type, Params}, format = default}|Res]);
read_writers([{Type, Params, Format}|Tail], Res) ->
    read_writers(Tail, [#chronica_backend{type = {Type, Params}, format = Format}|Res]);
read_writers([W | _Tail], _Res) ->
    throw({bad_config, {bad_writer, W}}).

search_format_id(_FormatId, []) -> false;
search_format_id(FormatId, [#chronica_format{format_id = Name} | Tail]) ->
    if
        FormatId =:= Name -> true;
        true              -> search_format_id(FormatId, Tail)
    end.

add_default_format(FormatsList) ->
    case search_format_id(default, FormatsList) of
        false -> [#chronica_format{format_id = default, format = "%Y-%M-%D %H:%Mi:%S.%Ms %PRIORITY %Pid [%Module:%Function:%Line]: %Message\n"}| FormatsList];
        true -> FormatsList
    end.

read_formats([], Res) ->
    add_default_format(Res);
read_formats([{Name, Str} | Tail], Res) when is_list(Str) ->
    read_formats(Tail, [#chronica_format{format_id = Name, format = Str} | Res]).

% read colors for tty
read_coloring(false) ->
    #chronica_coloring{colored = false};
read_coloring(true) ->
    #chronica_coloring{colored = true};
read_coloring({true, List}) when is_list(List) ->
    ColorsSpec = read_colors_spec(List, []),
    #chronica_coloring{colored = true, colors_spec = ColorsSpec};
read_coloring(Other) ->
    throw({bad_config, {bad_config_coloring, Other}}).

read_colors_spec([{_Name_id, []} | Colors], Colors_spec) ->
    read_colors_spec(Colors, Colors_spec);
read_colors_spec([{_Name_id, _Color} = Color_spec | Colors], Colors_spec) ->
    read_colors_spec(Colors, [Color_spec | Colors_spec]);
read_colors_spec([], Colors_spec) -> Colors_spec;
read_colors_spec(Other, _) ->
    throw({bad_config, {bad_config_coloring, {bad_colors_spec, Other}}}).


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
                CacheForOff = load_cache(CacheDir, ConfigHashForOff),
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
        BackendModules2 = BackendModules ++ [{tty, chronica_tty_backend}, {udp, chronica_udp_backend}, {tcp_con, chronica_tcp_con_backend}, {file, chronica_disk_log_backend}],

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
        NewRules = parse_rules(NewRulesConfig, lists:map(ClearFlowHandleFun, NewFlows), []),
        ConfigHash = config_hash(NewConfig),
        Cache = load_cache(CacheDir, ConfigHash),

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

get_appropriate_flows_exclusion(KeyModule, Rules) ->
    KeyStrModule = atom_to_list(KeyModule),
    lists:filter(fun(#rule{rule_str = RuleStr}) ->
                    ListBoolRule = rule_match3(RuleStr, KeyStrModule),
                    find_signal_flag(ListBoolRule)
                end, Rules).

rule_match3(Rule, Key) when is_list(Rule) ->
    rule_match2(Key, tokens(Rule));
rule_match3({"!", {Rule}}, Key) ->
    case rule_match3(Rule, Key) of
        true ->
            {false, global};
        Res ->
            not Res
    end;
rule_match3({"&", {Op1, Op2}}, Key) ->
    [rule_match3(Op1, Key), rule_match3(Op2, Key)];
rule_match3({"|", {Op1, Op2}}, Key) ->
    [rule_match3(Op1, Key), rule_match3(Op2, Key)].

find_signal_flag([{false,global} | _]) ->
    false;
find_signal_flag([Res | Tail]) ->
    case find_signal_flag(Res) of
        true ->
            find_signal_flag(Tail);
        _ ->
            false
    end;
find_signal_flag(_) ->
    true.

get_appropriate_flows(_Key, _Priority, null) -> [];
get_appropriate_flows([], _Priority, _Rules) -> [];
get_appropriate_flows(Key, Priority, Rules) when is_atom(Key) ->
    ?INT_DBG("get_appropriate_flows for: ~p", [Key]),
    KeyStr = atom_to_list(Key),
    F = fun(#rule{rule_str = RuleStr, log_priority = LPriority, flows = Flows}, R) ->
            case LPriority >= Priority andalso rule_match(RuleStr, KeyStr) of
                true -> [Flows, R];
                _else -> R
            end
        end,
    lists:usort(lists:flatten(lists:foldl(F, [], Rules)));
get_appropriate_flows([K|_] = Keys, Priority, Rules) when is_list(Keys), is_atom(K) ->
    ?INT_DBG("get_appropriate_flows for: ~p", [Keys]),
    lists:usort(lists:flatten([ get_appropriate_flows(Key, Priority, Rules) ||
                                Key <- Keys ]));
get_appropriate_flows(Key, _, _) ->
    erlang:error({bad_tags, Key}).

%% @doc Rule is list of tree, key is string
-spec rule_match(Rule :: rule(), Key :: string()) -> boolean().
rule_match(Rule, Key) when is_list(Rule) ->
    rule_match2(Key, tokens(Rule));
rule_match({"!", {Rule}}, Key) ->
    not rule_match(Rule, Key);
rule_match({"&", {Op1, Op2}}, Key) ->
    rule_match(Op1, Key) andalso rule_match(Op2, Key);
rule_match({"|", {Op1, Op2}}, Key) ->
    rule_match(Op1, Key) orelse rule_match(Op2, Key).

%% TODO: !?
tokens([$! | Tail]) ->
    {Res, TmpList} = tokens_(Tail),
    ["!", TmpList | Res];
tokens(Rule) ->
    {Res, TmpList} = tokens_(Rule),
    [TmpList | Res].
tokens_(Rule) ->
    lists:foldr(
      fun (Char, {Res, TmpList}) when Char == $* orelse Char == $? ->
              {[ [Char], TmpList | Res ], []};
          (Char, {Res, TmpList}) ->
              {Res, [Char | TmpList]}
      end, {[], []}, Rule).

rule_match2(Key, ["!" | TokensTail]) ->
    not rule_match2(Key, TokensTail);
rule_match2([_ | KeyTail], ["?" | TokensTail]) ->
    rule_match2(KeyTail, TokensTail);
rule_match2([], ["?" | _]) ->
    false;
rule_match2(Key, ["*" , [] | TokensTail]) ->
    rule_match2(Key, ["*" | TokensTail]);
rule_match2(Key, ["*" , RuleToken | TokensTail] = Rule) ->
    case string:str(Key, RuleToken) of
        0 -> false;
        N ->
            KeyLeft = string:substr(Key, N + string:len(RuleToken)),
            case rule_match2(KeyLeft, TokensTail) of
                false -> rule_match2(KeyLeft, Rule);
                R -> R
            end
    end;
rule_match2(_, ["*"]) ->
    true;
rule_match2(Key, [[] | TokensTail]) ->
    rule_match2(Key, TokensTail);
rule_match2([KeyChar | KeyTail], [[RuleChar | RuleTokenTail] | TokensTail]) ->
    case KeyChar =:= RuleChar of
        true -> rule_match2(KeyTail, [RuleTokenTail | TokensTail]);
        false -> false
    end;
rule_match2([], []) ->
    true;
rule_match2(_, _) ->
    false.

create_flows(ConfigFlows, WriterOptions, Backends) ->
    FlowsList = parse_flows(ConfigFlows, WriterOptions, Backends),
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

parse_flows([#chronica_flow{flow_id = Name, backends = Writers} | Tail], WriterOptions, Backends) ->
    [{Name, lists:usort(parse_flow(Writers, WriterOptions, Backends))} | parse_flows(Tail, WriterOptions, Backends)];
parse_flows([], _WriterOptions, _Backends) ->
    [];
parse_flows(BadFormated, _WriterOptions, _Backends) ->
    ?INT_ERR("Bad formated list of flows near ~p", [BadFormated]),
    erlang:throw({parse_flows, bad_format}).

parse_flow([#chronica_backend{type = {Type, OpenParam}, format = Format} = Output | Tail], WriterOptions, BackendModules) ->

    TTYEnabled = proplists:get_value(tty_enabled, WriterOptions, true),

    case TTYEnabled of
        false when Type == tty -> parse_flow(Tail, WriterOptions, BackendModules);
        _  ->

            OutputModule =
                case proplists:get_value(Type, BackendModules, undefined) of
                    undefined ->
                        ?INT_ERR("Unknown backend ~p, available: ~p", [Type, BackendModules]),
                        erlang:throw({parse_flow, {unknown_backend, Type}});
                    M ->
                        M
                end,

            FormatType = case Format of
                           undefined -> default;
                           _ -> Format
                         end,

            case code:ensure_loaded(chronica_format) of
                {module, _} -> ok;
                {error, Err} -> erlang:throw({parse_flow, load_format, Err})
            end,

            case erlang:function_exported(chronica_format, FormatType, 1) of
                true -> ok;
                false ->
                    ?INT_ERR("Bad format of output ~p near ~p", [Output, FormatType]),
                    erlang:throw({parse_flow, bad_format_type})
            end,

            case chronica_gen_backend:open(OutputModule, OpenParam, WriterOptions) of
                {ok, Handle} ->
                    FlowHandle = #flow_handle{id = Handle, format_type = FormatType, output_module = OutputModule,
                                              open_params = OpenParam, writer_options = WriterOptions},
                    [FlowHandle | parse_flow(Tail, WriterOptions, BackendModules)];
                {error, _Error} ->
                    ?INT_ERR("Error open output: ~p Reason: ~p", [Output, _Error]),
                    erlang:throw({parse_flow, cant_open_output})
            end
    end;

parse_flow([], _WriterOptions, _Backends) ->
    [];
parse_flow(BadFormated, _WriterOptions, _Backends) ->
    ?INT_ERR("Bad formated output list ~p", [BadFormated]),
    erlang:throw({parse_flow, bad_format_output_list}).

parse_rules([#chronica_rule{in_work = false} | T], Flows, Rules) -> parse_rules(T, Flows, Rules);
parse_rules([#chronica_rule{id = RuleId, mask = RuleStr, priority = RulePriority, flow_ids = RuleFlowIds, in_work = true} | T], Flows, Rules) ->
    case {lists:keyfind(RuleId, #chronica_rule.id, T), RuleStr =:= "", not is_list(RuleStr)} of
        {false, false, false} ->

            NewRuleStr = parse_rule_pattern(RuleStr),

            NewRule = #rule{
                        id = RuleId,
                        rule_str = NewRuleStr,
                        log_priority =  get_priority_num(RulePriority),
                        flows = parse_flowids(RuleFlowIds, Flows, [])},

            parse_rules(T, Flows, [ NewRule | Rules]);
        {_, _, true} ->
            ?INT_ERR("rule id should be string: ~p, skip", [RuleStr]),
            erlang:throw({parse_rules, ruleid_isnot_atom});
        {_, true, _} ->
            ?INT_ERR("rule \"\" is forbidden: ~p, skip", [RuleStr]),
            erlang:throw({parse_rules, forbidden_rule});
        {Type, _, _} when Type /= false ->
            ?INT_ERR("doubling of rule: ~p, skip", [RuleStr]),
            erlang:throw({parse_rules, rule_doubling})
    end;

parse_rules([], _Flows, Rules) ->
    Rules;

parse_rules(BadFormated, _, _Rules) ->
    ?INT_ERR("Bad formated rule list ~p", [BadFormated]),
    erlang:throw({parse_rules, bad_rule_list}).

parse_flowids([], _, Res) ->
    Res;
parse_flowids([FlowId | Tail], Flows, Res) ->
    NewFlow = get_flow_by_name(FlowId, Flows),
    parse_flowids(Tail, Flows, lists:merge(Res, NewFlow));
parse_flowids(BadFormated, _Flows, _) ->
    ?INT_ERR("Bad formated list of properties near ~p", [BadFormated]),
    erlang:throw({parse_rule, bad_rule_prop_list}).

get_priority_num(PriorityAtom) ->
    case PriorityAtom of
        nothing -> ?P_NOTHING;
        error -> ?P_ERROR;
        warning -> ?P_WARNING;
        info -> ?P_INFO;
        trace -> ?P_TRACE;
        debug -> ?P_DEBUG;
        _ ->
            ?INT_ERR("unknown priority atom ~p", [PriorityAtom]),
            erlang:throw({get_priority_num, unknown_priority})
    end.

get_flow_by_name(Name, ConfigFlows) ->
    case lists:keyfind(Name, 1, ConfigFlows) of
        false ->
            ?INT_ERR("~p flow not found", [Name]),
            erlang:throw({get_flow_by_name, flow_not_found});
        {Name, ResList} when is_list(ResList)-> ResList;
        Unknown->
            ?INT_ERR("invalid flow (not list) ~p", [Unknown]),
            erlang:throw({get_flow_by_name, bad_flow_list})
    end.

parse_formats([], Res, _) -> Res;
parse_formats([#chronica_format{format_id = Name, format = UserFormat} | Tail], Res, ColorsConfig) when is_atom(Name) and is_list(UserFormat) ->
    parse_formats(Tail, [chronica_format_creator:create_format_function(Name, UserFormat, ColorsConfig) | Res], ColorsConfig);
parse_formats(BadFormated, _, _) ->
    ?INT_ERR("invalid formats param ~p", [BadFormated]),
    erlang:throw({parse_formats, bad_format}).

get_log_root(Str, Now) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:now_to_local_time(Now),
    ReOpts = [unicode, global, {return, list}],
    Str1 = re:replace(Str,  "\\<Node\\>",   atom_to_list(node()), ReOpts),
    Str2 = re:replace(Str1, "\\<Year\\>",   indented_str(4, Y),   ReOpts),
    Str3 = re:replace(Str2, "\\<Month\\>",  indented_str(2, M),   ReOpts),
    Str4 = re:replace(Str3, "\\<Day\\>",    indented_str(2, D),   ReOpts),
    Str5 = re:replace(Str4, "\\<Hour\\>",   indented_str(2, H),   ReOpts),
    Str6 = re:replace(Str5, "\\<Minute\\>", indented_str(2, Mi),  ReOpts),
    re:replace(Str6, "\\<Second\\>", indented_str(2, S), ReOpts).

get_data_root(Str) ->
    re:replace(Str, "\\<Node\\>", atom_to_list(node()),
               [unicode, global, {return, list}]).

indented_str(I, N) when is_integer(N) and is_integer(I) ->
    Str = integer_to_list(N),
    case length(Str) of
        K when K >=I -> Str;
        K when I > K -> add_n_chars(I - K, Str, $0)
    end.

add_n_chars(0, Str, _) -> Str;
add_n_chars(N, Str, Char) -> add_n_chars(N-1, [Char | Str], Char).

add_application(App, RApps, Rules, Cache, Detail_info) ->
    add_application(App, RApps, Rules, Cache, Detail_info, undefined).

add_application(App, RApps, Rules, Cache, Detail_info, TickFun) ->
    ?INT_DBG("-> rq add_application ~p \\ ~p", [App, RApps]),
    try
        case lists:member(App, RApps) of
            true -> {ok, {RApps, Cache}};
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
                                    case add_application(A, Added, Rules, CacheAcc, Detail_info, TickFun) of
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
        end
    catch
        throw:{Err, NRA} ->
            ?INT_ERR("reg ~p failed \\ ~p~n", [App, NRA]),
            {Err, NRA}
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
                ?INT_DBG("Registered (from cache) \\ ~p~n", [[App | RApps]]),
                {ok, {[App | RApps], Cache}};
            % no cache or old cache
            _Cached ->
                AppModulesCode = case Detail_info of
                    true ->
                        io:format("Compiling the cache: [~p] ", [App]),
                        T1 = erlang:system_time(milli_seconds),
                        Apps = generate_app_iface_modules(App, Rules),
                        T2 = erlang:system_time(milli_seconds),
                        case Apps of
                            [] -> io:format("skipped~n");
                            _ -> io:format("~pms done.~n", [T2 - T1])
                        end,
                        Apps;
                    false ->
                        generate_app_iface_modules(App, Rules)
                end,
                load_app_iface_modules(AppModulesCode),
                ?INT_DBG("Registered \\ ~p~n", [[App | RApps]]),
                {ok, {[App | RApps], [{App, {AppHash, AppModulesCode}} | proplists:delete(App, Cache)]}}
        end
    catch
        _:Error ->
            ?INT_ERR("Add application ~p failed cause: ~p~n~p", [App, Error, erlang:get_stacktrace()]),
            {Error, {RApps, Cache}}
    end.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

generate_app_iface_modules(App, Rules) ->
    AppModules =
        case application:get_key(App, modules) of
            {ok, L2} -> L2;
            _ -> []
        end,

    lists:foldl(
        fun (M, Acc) ->
            ?INT_DBG("Generating iface module for ~p", [M]),
            NewAcc =
                case generate_iface_module(M, Rules) of
                    undefined ->
                        ?INT_DBG("not generated", []),
                        Acc;
                    Code      ->
                        ?INT_DBG("generated", []),
                        [{M, Code}|Acc]
                end,
            NewAcc
        end, [], AppModules).

generate_iface_module(ModuleName, Rules) ->
    TagList =
        case (catch ModuleName:get_log_tags()) of
            Tags when is_list(Tags) -> Tags;
            _ -> []
        end,

    case TagList of
        [] ->
            undefined;
        _ ->
            timer:sleep(5), % reduce overload
            IfaceName = pt_chronica:generate_module_iface_name(ModuleName),
            AST  = pt_lib:create_clear_ast(IfaceName),

            DefaultClause =
                ast("(Priority, Key, Module, Line, File, Function, Format, Args, Self, Now) ->
                        chronica_core:sync_fast_log({fast_log_message, Priority, Format, Args, Module, Line, File, Function, Self, Now, @IfaceName, Key}).", 0),
            Rules2 = get_appropriate_flows_exclusion(ModuleName, Rules),

            Flows = [
                {Tags2, [{Priority, get_appropriate_flows(Tags2, Priority, Rules2)} || Priority <- [?P_ERROR, ?P_WARNING, ?P_INFO, ?P_TRACE, ?P_DEBUG]]}

                ||

                Tags2 <- TagList
            ],
            Clauses = generate_clauses(TagList, Flows) ++ [DefaultClause],
            AST2 = pt_lib:add_function(AST, ast("log_fast [...$Clauses...].", 0)),

            DefaultClause2 = ast("([], Priority) -> not_found.", 0),

            Clauses2 = generate_clauses_for_get_flows(TagList, Flows) ++ [DefaultClause2],
            AST3 = pt_lib:add_function(AST2, ast("get_flows [...$Clauses2...].", 0)),
            case compile:forms(AST3, [binary, return_errors]) of
                {ok, IfaceName, Binary} ->
                    ?INT_DBG("Compiled iface module for ~p: ~p", [ModuleName, IfaceName]),
                    {IfaceName, Binary};
                Error ->
                    ?INT_ERR("log interface module compilation error: ~p", [Error]),
                    erlang:error(log_iface_module_compilation_error, Error)
            end
    end.

generate_clauses(TagList, FlowsList) ->
    lists:foldl(
    fun (Tags, Acc) ->
        lists:foldl(
            fun (Priority, Acc2) ->
                {_, FlowsPriorities} = lists:keyfind(Tags, 1, FlowsList),
                {_, Flows} = lists:keyfind(Priority, 1, FlowsPriorities),
                case Flows of
                    [] ->
                        [ast("(@Priority = Priority, @Tags, Module, Line, File, Function, Format, Args, Self, Now) -> ok.", 0) | Acc2];
                    _ ->
                        Acc2
                end
            end, Acc, [?P_ERROR, ?P_WARNING, ?P_INFO, ?P_TRACE, ?P_DEBUG])
    end, [], TagList).

generate_clauses_for_get_flows(TagList, FlowsList) ->
    [
        begin
            ast("(@Tags, @Priority) -> @Flows.", 0)
        end
    ||
        Tags <- TagList,
        {Tags2, FlowsPriorities} <- FlowsList,
        {Priority, Flows} <- FlowsPriorities,
        Priority2 <- [?P_ERROR, ?P_WARNING, ?P_INFO, ?P_TRACE, ?P_DEBUG],
        Priority2 == Priority,
        Tags2 == Tags
    ].

set_configured_true() ->
    pt_recompilable:recompile_cur_module(
        chronica_status,
        fun (AST, _Options) ->
            pt_lib:replace(AST,
                ast_pattern("configured() -> ...$_... .", Line),
                ast("configured() -> true.", Line))
        end).

parse_rule_pattern(P) ->
    P1 = parse_brakets(P),
    P2 = parse_binary_op(P1, "|"),
    P3 = parse_binary_op(P2, "&"),
    P4 = parse_unary_op(P3, "!"),
    P4.

parse_brakets("(" ++ P) ->
    {SubP, Tail} = split_pair_braket(P),
    [parse_brakets(SubP)|parse_brakets(Tail)];
parse_brakets([C|P]) ->
    [C|parse_brakets(P)];
parse_brakets([]) -> [].

parse_binary_op([{_, _} = Op1], Op) ->
    parse_binary_op(Op1, Op);
parse_binary_op({Op1, {P1, P2}}, Op) ->
    {Op1, {parse_binary_op(P1, Op), parse_binary_op(P2, Op)}};
parse_binary_op({Op1, {P1}}, Op) ->
    {Op1, {parse_binary_op(P1, Op)}};
parse_binary_op(List, Op) when is_list(List) ->
    case string:tokens(List, Op) of
        [First] ->
            lists:map(
                fun ([E]) when is_list(E) -> parse_binary_op(E, Op);
                    (E) -> parse_binary_op(E, Op)
                end, First);
        [First|Tokens] ->
            lists:foldl(
                fun
                    ([E], Cur) when is_list(E) ->
                        {Op, {Cur, parse_binary_op(E, Op)}};
                    (E, Cur) ->
                        {Op, {Cur, parse_binary_op(E, Op)}}
                end, parse_binary_op(First, Op), Tokens)
    end;
parse_binary_op(E, _Op) -> E.

parse_unary_op([{_, _} = Op1], Op) ->
    parse_unary_op(Op1, Op);
parse_unary_op({Op1, {P1, P2}}, Op) ->
    {Op1, {parse_unary_op(P1, Op), parse_unary_op(P2, Op)}};
parse_unary_op({Op1, {P1}}, Op) ->
    {Op1, {parse_unary_op(P1, Op)}};
parse_unary_op(List, Op) when is_list(List) ->
    Str = string:strip(List),
    case lists:prefix(Op, Str) of
        true  -> {Op, {parse_unary_op(Str -- Op, Op)}};
        false -> List
    end.

split_pair_braket(P) -> split_pair_braket_(P, [], 0).
split_pair_braket_([], Acc1, _) -> throw({no_pair_braket, lists:reverse(Acc1)});
split_pair_braket_(")" ++ Tail, Acc1, 0) -> {lists:reverse(Acc1), Tail};
split_pair_braket_(")" ++ Tail, Acc1, BN) when BN > 0 -> split_pair_braket_(Tail, [$)|Acc1], BN - 1);
split_pair_braket_(")" ++ Tail, _Acc1, _BN) -> throw({invalid_braket, Tail});
split_pair_braket_("(" ++ Tail, Acc1, BN) -> split_pair_braket_(Tail, [$(|Acc1], BN + 1);
split_pair_braket_([C|Tail], Acc1, BN) -> split_pair_braket_(Tail, [C|Acc1], BN).

depended_from_chronica(chronica) ->
    true;
depended_from_chronica(App) ->
    CheckApplicationFun =
        fun({ok, DependedApplications}) ->
                lists:member(chronica, DependedApplications);
           (_) ->
                false
        end,
    CheckApplicationFun(application:get_key(App, applications)) orelse CheckApplicationFun(application:get_key(App, included_applications)).

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
    lists:foldl(
        fun (App, {RAAcc, CacheAcc}) ->
            case add_application(App, RAAcc, NewRules, CacheAcc, Detail_info, TickFun) of
                {ok, NRA} -> NRA;
                {AAErr, _NRA} -> throw(AAErr)
            end
        end,
        {[], Cache},
        RegApps).

load_cache(CacheDir, ConfigHash) ->
    Filename = cache_filename(CacheDir, ConfigHash),
    case file:read_file(Filename) of
        {ok, Binary} ->
            try
                Cache = erlang:binary_to_term(Binary),
                {BeamsCache, Cache1} =
                    case Cache of
                        [{beam_cache, Hash} | TailCache] ->
                            {{beam_cache, Hash}, TailCache};
                        _ ->
                            {{beam_cache, undef}, Cache}
                    end,
                case BeamsCache == cache_beams() of
                    true ->
                        Cache1;
                    false ->
                        []
                end
            catch
                _:_ ->
                    ?INT_ERR("Broken chronica cache in the file (~p)", [Filename]),
                    []
            end;
        {error, enoent} -> [];
        {error, Error} ->
            ?INT_ERR("Load chronica cache (~p) error: ~p", [Filename, Error]),
            []
    end.

cache_beams() ->
    Dir = filename:dirname(code:which(chronica)),
    Beams = filelib:wildcard(filename:join(Dir, "*.beam")),
    VSNs = lists:foldl(
        fun(Beam, Acc) ->
            try
                {ok, {_, [{attributes, Attr}]}} = beam_lib:chunks(Beam, [attributes]),
                case proplists:get_value(vsn, Attr) of
                    undefined -> Acc;
                    [VSN] -> [VSN | Acc];
                    _ -> Acc
                end
            catch
                _:_ ->
                    Acc
            end
        end, [], Beams),
    Hash = erlang:phash2(VSNs, 999999),
    {beam_cache, lists:flatten(io_lib:format("~6.10.0B", [Hash]))}.

save_cache(_CacheDir, _ConfigHash, []) -> ok;
save_cache(CacheDir, ConfigHash, Cache) ->
    Filename = cache_filename(CacheDir, ConfigHash),
    CacheBeams = cache_beams(),
    Binary = erlang:term_to_binary([CacheBeams | Cache]),
    Size = erlang:byte_size(Binary),
    ?INT_DBG("Save chronica cache (~b bytes) to ~p", [Size, Filename]),
    case file:write_file(Filename, Binary) of
        ok ->
            ok;
        {error, Error} ->
            ?INT_ERR("Save cache to ~10000000p error: ~p", [Filename, Error]),
            {error, Error}
    end.

cache_filename(CacheDir, ConfigHash) ->
    S = bin2hex(erlang:binary_to_list(ConfigHash)),
    filename:join(CacheDir, S).

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

bin2hex(Hex) when is_list(Hex) ->
     F = fun (16#0) -> $0;
             (16#1) -> $1;
             (16#2) -> $2;
             (16#3) -> $3;
             (16#4) -> $4;
             (16#5) -> $5;
             (16#6) -> $6;
             (16#7) -> $7;
             (16#8) -> $8;
             (16#9) -> $9;
             (16#A) -> $A;
             (16#B) -> $B;
             (16#C) -> $C;
             (16#D) -> $D;
             (16#E) -> $E;
             (16#F) -> $F
         end,

    F2 = fun(I, A) ->
        H = F(I bsr 4),
        L = F(I band 16#F),
        [H|[L|A]]
     end,

    lists:foldl(F2, [], lists:reverse(Hex)).


-define(d2012_11_16_1_25_41, 63520248341000000).

generate_name(Prefix) ->
    Now = {_, _, M} = os:timestamp(),
    DT = calendar:now_to_universal_time(Now),
    N = calendar:datetime_to_gregorian_seconds(DT) * 1000000 + M - ?d2012_11_16_1_25_41,

    lists:flatten(io_lib:format("~s~.36.0b", [Prefix, N])).

-module(chronica_config).

-include("chronica_int.hrl").
-include("chronica_config.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").
-include_lib("pt_lib/include/pt_lib.hrl").

-export(
   [
    read/1,
    parse_flows/3,
    parse_rules/2,
    validate/1
   ]).

-spec read(Now :: term()) -> {ok, #chronica_config{}} | {error, term()}.
read(Now) ->
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

parse_flows([#chronica_flow{flow_id = Name, backends = Writers} | Tail], WriterOptions, Backends) ->
    [{Name, lists:usort(parse_flow(Writers, WriterOptions, Backends))} | parse_flows(Tail, WriterOptions, Backends)];
parse_flows([], _WriterOptions, _Backends) ->
    [];
parse_flows(BadFormated, _WriterOptions, _Backends) ->
    ?INT_ERR("Bad formated list of flows near ~p", [BadFormated]),
    erlang:throw({parse_flows, bad_format}).

parse_flow([#chronica_backend{type = {Type, OpenParam}, format = Format} = Output | Tail], WriterOptions, BackendModules) ->
    case proplists:get_value(tty_enabled, WriterOptions, true) of
        false when Type == tty -> parse_flow(Tail, WriterOptions, BackendModules);
        _  ->
            case proplists:get_value(Type, BackendModules, undefined) of
                undefined ->
                    parse_flow(Tail, WriterOptions, BackendModules);
                M ->
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
                    case chronica_gen_backend:open(M, OpenParam, WriterOptions) of
                        {ok, Handle} ->
                            FlowHandle = #flow_handle{id = Handle, format_type = FormatType, output_module = M,
                                              open_params = OpenParam, writer_options = WriterOptions},
                            [FlowHandle | parse_flow(Tail, WriterOptions, BackendModules)];
                        {error, _Error} ->
                            ?INT_ERR("Error open output: ~p Reason: ~p", [Output, _Error]),
                            erlang:throw({parse_flow, cant_open_output})
                    end
            end
    end;

parse_flow([], _WriterOptions, _Backends) ->
    [];
parse_flow(BadFormated, _WriterOptions, _Backends) ->
    ?INT_ERR("Bad formated output list ~p", [BadFormated]),
    erlang:throw({parse_flow, bad_format_output_list}).

parse_rules(Rules, Flows) -> parse_rules(Rules, Flows, []).
parse_rules([#chronica_rule{in_work = false} | T], Flows, Rules) -> parse_rules(T, Flows, Rules);
parse_rules([#chronica_rule{id = RuleId, mask = RuleStr, priority = RulePriority, flow_ids = RuleFlowIds, in_work = true} | T], Flows, Rules) ->
    case {lists:keyfind(RuleId, #chronica_rule.id, T), RuleStr =:= "", not is_list(RuleStr)} of
        {false, false, false} ->

            NewRuleStr = chronica_rule:parse_pattern(RuleStr),

            NewRule = #rule{
                        id = RuleId,
                        rule_str = NewRuleStr,
                        log_priority = get_priority_num(RulePriority),
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

get_priority_num(nothing) -> ?P_NOTHING;
get_priority_num(error)   -> ?P_ERROR;
get_priority_num(warning) -> ?P_WARNING;
get_priority_num(info)    -> ?P_INFO;
get_priority_num(trace)   -> ?P_TRACE;
get_priority_num(debug)   -> ?P_DEBUG;
get_priority_num(PriorityAtom) ->
    ?INT_ERR("unknown priority atom ~p", [PriorityAtom]),
    erlang:throw({get_priority_num, unknown_priority}).

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

validate(Config) -> chronica_config_validation:validate(Config).

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
add_n_chars(N, Str, Char) -> add_n_chars(N - 1, [Char | Str], Char).

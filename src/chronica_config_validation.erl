%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_config_validation).

-export([validate/1, validate_internal_logger/1]).

-include("chronica_config.hrl").

validate(#chronica_config{
                    rules = Rules,
                    flows = Flows,
                    formats = Formats,
                    colors = Colors,
                    active = Power,
                    detail_info = Detail_info,
                    rotate_at_start = RotateAtStartup,
                    internal_logger = InternalLogger,
                    log_root = LogRoot,
                    data_root = DataRoot,
                    max_file_size = MaxFileSize,
                    max_file_num = MaxFileNum,
                    tty_enabled = TTYEnabled
                }) ->
    try
        Colors = validate_colors(Colors),
        FormatList = validate_formats(Formats),
        FlowList = validate_flows(Flows, FormatList),
        _RuleList = validate_rules(Rules, FlowList),

        validate_active(Power),
        validate_detail_info(Detail_info),
        validate_rotate_at_startup(RotateAtStartup),
        validate_internal_logger(InternalLogger),
        validate_log_root(LogRoot),
        validate_data_root(DataRoot),
        validate_sizes(MaxFileSize, MaxFileNum),
        validate_tty_enabled(TTYEnabled)
    catch
        throw:E -> throw({bad_config, E})
    end,
    ok.

validate_rules([], _FlowList) ->
    throw({bad_rules, []});
validate_rules(Rules, FlowList) ->
    validate_rules2(Rules, FlowList, []).

validate_rules2([], _FlowList, RuleList) -> RuleList;
validate_rules2([#chronica_rule{id = Id, mask = M, priority = P, flow_ids = Fs, in_work = InWork} | Tail], FlowList, RuleList) when erlang:is_list(M) and erlang:is_list(Fs) and ((InWork =:= true) or (InWork =:= false)) ->
    try
        lists:map(
            fun (FName) ->
                case lists:member(FName, FlowList) of
                    true -> ok;
                    false -> throw({bad_flow_id, FName})
                end
            end, Fs),
        case lists:member(P, [debug, trace, info, warning, error]) of
            true -> ok;
            false -> throw({bad_priority, P})
        end
    catch
        throw:E -> throw({bad_rule, {Id, E}})
    end,
    case erlang:is_atom(Id) of
        true -> ok;
        false -> throw({bad_rule_id, Id})
    end,
    case lists:member(Id, RuleList) of
        true -> throw({doubling_rules, Id});
        false -> ok
    end,
    validate_rules2(Tail, FlowList, [Id | RuleList]);
validate_rules2([R | _], _FlowList, _RuleList) ->
    throw({bad_rule, R});
validate_rules2(R, _FlowList, _RuleList) ->
    throw({bad_rule_list, R}).

validate_flows([], _) -> throw({bad_flow_list, []});
validate_flows(Flows, FormatList) ->
    validate_flows2(Flows, FormatList, []).

validate_flows2([], _FormatList, FlowList) -> FlowList;
validate_flows2([#chronica_flow{flow_id = N} | Tail], FormatList, FlowList) when erlang:is_atom(N) ->
    case lists:member(N, FlowList) of
        true -> throw({doubling_flows, N});
        false -> ok
    end,
    validate_flows2(Tail, FormatList, [N | FlowList]);
validate_flows2([F | _], _FormatList, _FlowList) ->
    throw({bad_flow, F});
validate_flows2(F, _FormatList, _FlowList) ->
    throw({bad_flow_list, F}).

validate_colors(#chronica_coloring{colored = Colored, colors_spec = ColorsSpec} = Chronica_colors) ->
    case Colored of
        false -> ok;
        true -> validate_colors_spec(ColorsSpec, [])
    end,
    Chronica_colors;
validate_colors(Other) ->
    throw({bad_coloring, Other}).

validate_colors_spec([], ColorsSpecList) -> ColorsSpecList;
validate_colors_spec([{Data, Colors} = Head | Tail], ColorsSpecList) when erlang:is_atom(Data) ->
    case lists:keyfind(Data, 1, ColorsSpecList) of
        {Data, _} -> throw({doubling_colors_spec, {Data, Colors}});
        false -> ok
    end,
    validate_colors_spec(Tail, [Head | ColorsSpecList]);
validate_colors_spec(Other, _) ->
    throw({bad_colors_spec, Other}).

validate_formats(Formats) ->
    validate_formats2(Formats, []).

validate_formats2([], FormatsList) -> FormatsList;
validate_formats2([#chronica_format{format_id = N, format = F} | Tail], FormatsList) when erlang:is_atom(N) and erlang:is_list(F) ->
    case lists:member(N, FormatsList) of
        true -> throw({doubling_formats, N});
        false -> ok
    end,
    validate_formats2(Tail, [N | FormatsList]);
validate_formats2([F | _Tail], _)  ->
    throw({bad_config, {bad_format, F}});
validate_formats2(F, _)  ->
    throw({bad_config, {bad_format_list, F}}).

validate_detail_info(true) -> ok;
validate_detail_info(false) -> ok;
validate_detail_info(Detail_info) -> throw({bad_detail_info_param, Detail_info}).

validate_active(true) -> ok;
validate_active(false) -> ok;
validate_active(Power) -> throw({bad_active_param, Power}).

validate_internal_logger(InternalLogger) when erlang:is_list(InternalLogger) ->
    lists:map(
        fun ({file, FN, {SN, NN}, Filter})
                    when erlang:is_list(FN),
                         erlang:is_integer(SN),
                         erlang:is_integer(NN),
                         (SN > 0),
                         (NN > 0)
                ->
                    try
                        validate_il_filter(Filter)
                    catch
                        throw:Err -> throw({bad_internal_logger, Err})
                    end;
            ({tty, Filter}) ->
                    try
                        validate_il_filter(Filter)
                    catch
                        throw:Err -> throw({bad_internal_logger, Err})
                    end;
            (P) -> throw({bad_internal_logger, P})
        end,
        InternalLogger),
    ok;
validate_internal_logger(InternalLogger) ->
    throw({bad_internal_logger_list, InternalLogger}).

validate_il_filter(none) -> ok;
validate_il_filter(error) -> ok;
validate_il_filter(warning) -> ok;
validate_il_filter(info) -> ok;
validate_il_filter(debug) -> ok;
validate_il_filter(F) -> throw({bad_filter, F}).

validate_log_root(LogRoot) when erlang:is_list(LogRoot) and (LogRoot =/= []) ->
    ok;
validate_log_root(LogRoot) ->
    throw({bad_log_root, LogRoot}).

validate_data_root(DataRoot) when erlang:is_list(DataRoot) and (DataRoot =/= []) ->
    ok;
validate_data_root(DataRoot) ->
    throw({bad_data_root, DataRoot}).

validate_sizes(MaxFileSize, MaxFileNum) ->
    case MaxFileSize of
        S when erlang:is_integer(S) and (S > 0)  -> ok;
        _ -> throw({bad_max_file_size, MaxFileSize})
    end,
    case MaxFileNum of
        N when erlang:is_integer(N) and (N > 0)  -> ok;
        _ -> throw({bad_max_file_size, MaxFileSize})
    end,
    ok.

validate_rotate_at_startup(true) -> ok;
validate_rotate_at_startup(false) -> ok;
validate_rotate_at_startup(P) -> throw({bad_rotate_at_start_up, P}).

validate_tty_enabled(true) -> ok;
validate_tty_enabled(false) -> ok;
validate_tty_enabled(P) -> throw({bad_tty_enabled, P}).

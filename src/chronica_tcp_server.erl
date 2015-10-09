%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_tcp_server).

-behaviour(gen_server).

-include("chronica_int.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").
%% API
-export([start_link/2, accepted/1, get_listen_port/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(s,
        {
            port :: integer(),
            listen_socket,
            accepting_connection_monitor,
            accepting_connection,
            epmd_con
        }).

-define(DEFAULT_TCP_BACKLOG, 10).
-define(CONNECTION_RESTART_TIMER, 1000).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Host :: any | nonempty_string() | inet:ip_address(), Port :: integer()) -> {ok, Pid :: pid()} | ignore | {error, Error :: term()}.

start_link(Host, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port], []).

accepted(ConPid) ->
    gen_server:cast(?MODULE, {accepted, ConPid}).

get_listen_port() ->
    try
        gen_server:call(?MODULE, get_listen_port)
    catch
        _:_ -> undefined
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) ->
                    {ok, State :: #s{}} |
                    {ok, State :: #s{}, Timeout :: non_neg_integer()} |
                    ignore |
                    {stop, Reason :: term()}.

init([Host, Port]) ->
    erlang:process_flag(trap_exit, true),
    try host_to_ip(Host) of
        IP ->
            case catch gen_tcp:listen(Port, [binary, {ifaddr, IP}, {packet, 0}, {active, true}, {nodelay, false}, {keepalive, true}, {backlog, ?DEFAULT_TCP_BACKLOG}, {reuseaddr, true}]) of
                {ok, ListenSocket} ->
                    {ok, Port2} = inet:port(ListenSocket),
                    case chronica_epmd_iface:alive2(Port2, hidden, get_grabber_node_name(), infinity) of
                        {ok, Ss} ->
                            case chronica_tcp_connection:start(ListenSocket, self()) of
                                {ok, ConPid} ->
                                    Mon = erlang:monitor(process, ConPid),
                                    io:format("~p:~b TCP connection ", [Host, Port2]),
                                    {ok, #s{port = Port2, listen_socket = ListenSocket, accepting_connection_monitor = Mon, accepting_connection = ConPid, epmd_con = Ss}};
                                Error2 ->
                                    gen_tcp:close(Ss),
                                    ?INT_ERR("Can't start accepting connection cause: ~10000p", [Error2]),
                                    {stop, Error2}
                            end;
                        {error, Error3} -> {stop, Error3}
                    end;
                Error ->
                    ?INT_ERR("Can't start listening of tcp port ~1000p, cause: ~10000p", [Port, Error]),
                    {stop, Error}
            end
    catch
        throw:{error, Error4} ->
            ?INT_ERR("Can't get IP by host ~1000p, cause: ~10000p", [Host, Error4]),
            {stop, Error4}
    end.

-spec handle_call(Request :: term(), From :: term(), State :: #s{}) ->
                                   {reply, Reply :: term(), State :: #s{}} |
                                   {reply, Reply :: term(), State :: #s{}, Timeout :: non_neg_integer()} |
                                   {noreply, State :: #s{}} |
                                   {noreply, State :: #s{}, Timeout :: non_neg_integer()} |
                                   {stop, Reason :: term(), Reply :: term(), State :: #s{}} |
                                   {stop, Reason :: term(), State :: #s{}}.

handle_call(get_listen_port, _From, State = #s{port = Port}) ->
    {reply, Port, State};

handle_call(_Request, _From, State) ->
    ?INT_ERR("Unhandled call: ~10000p", [_Request]),
    {reply, {error, unknown_request}, State}.

-spec handle_cast(Msg :: term(), State :: #s{}) ->
                                  {noreply, State :: #s{}} |
                                  {noreply, State :: #s{}, Timeout :: non_neg_integer()} |
                                  {stop, Reason :: term(), State :: #s{}}.

handle_cast({accepted, ConPid}, State = #s{accepting_connection = ConPid, accepting_connection_monitor = Mon, listen_socket = ListenSocket}) ->
    erlang:demonitor(Mon, [flush]),
    case chronica_tcp_connection:start(ListenSocket, self()) of

        {ok, NewConPid} ->
            NewMon = erlang:monitor(process, NewConPid),
            {noreply, State#s{accepting_connection_monitor = NewMon, accepting_connection = NewConPid}};

        Error2 ->
            ?INT_ERR("Can't start accepting connection cause: ~10000p", [Error2]),
            {stop, Error2, State#s{accepting_connection = undefined, accepting_connection_monitor = undefined}}
    end;

handle_cast(_Msg, State) ->
    ?INT_ERR("Unhandled cast: ~10000p", [_Msg]),
    {noreply, State}.

-spec handle_info(Info :: term(), State :: #s{}) ->
                                   {noreply, State :: #s{}} |
                                   {noreply, State :: #s{}, Timeout :: non_neg_integer()} |
                                   {stop, Reason :: term(), State :: #s{}}.

handle_info({'DOWN', MonitorRef, _, _, _}, State = #s{port = Port, listen_socket = ListenSocket, accepting_connection_monitor = MonitorRef, accepting_connection = OldCon}) ->
    ?INT_ERR("Received DOWN from accepting connection ~10000p while listening to ~p", [OldCon, Port]),
    timer:sleep(?CONNECTION_RESTART_TIMER),
    case chronica_tcp_connection:start(ListenSocket, self()) of

        {ok, ConPid} ->
            Mon = erlang:monitor(process, ConPid),
            {noreply, State#s{accepting_connection_monitor = Mon, accepting_connection = ConPid}};

        Error2 ->
            ?INT_ERR("Can't start accepting connection cause: ~10000p", [Error2]),
            {stop, Error2, State#s{accepting_connection = undefined, accepting_connection_monitor = undefined}}
    end;

handle_info(listen_socket_closed, State = #s{}) ->
    {stop, listen_socket_closed, State};

handle_info(_Info, State) ->
    ?INT_ERR("Unhandled info: ~10000p", [_Info]),
    {noreply, State}.

-spec terminate(Reason :: term(), State :: #s{}) -> ok.

terminate(Reason, _State = #s{port = Port}) ->
    case Reason of
        normal   -> ok;
        shutdown -> ok;
        _ -> ?INT_ERR("TCP server at ~p terminated ~10000p", [Port, Reason])
    end,
    ok.

-spec code_change(OldVsn :: term(), State :: #s{}, Extra :: term) ->
    {ok, NewState :: #s{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_grabber_node_name() ->
    [Name | _] = string:tokens(erlang:atom_to_list(node()), "@"),
    "log_grabber_" ++ Name.

host_to_ip(any) ->
    any;

host_to_ip({A, B, C, D} = Host) when A >= 0, A =< 255, B >= 0, B =< 255, C >= 0, C =< 255, D >= 0, D =< 255 ->
    Host;

host_to_ip({A, B, C, D, E, F, G, H} = Host) when A >= 0, A =< 65535, B >= 0, B =< 65535, C >= 0, C =< 65535, D >= 0, D =< 65535,
                                                 E >= 0, E =< 65535, F >= 0, F =< 65535, G >= 0, G =< 65535, H >= 0, H =< 65535 ->
    Host;

host_to_ip(Host) ->
    try
        case inet:getaddr(Host, inet) of
            {ok, IP4} ->
                throw({result, IP4});
            _ ->
                ok
        end,
        case inet:getaddr(Host, inet6) of
            {ok, IP6} ->
                throw({result, IP6});
            _ ->
                ok
        end,
        throw({error, {invalid_host, Host}})
    catch
        _:{result, Result} ->
            Result
    end.
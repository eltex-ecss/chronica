%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_tcp_connection).

-behaviour(gen_server).

-include("chronica_int.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").

%% API
-export([start_link/2, start/2, write/2, close/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(s, {
          state = accept :: accept | waiting_settings | sending_data,
          listen_socket,
          man_mon,
          manager,
          socket,
          name,
          mask,
          assembling_buff = []
         }).

-define(reaccept_timeout, 5000).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(ListenSocket :: term(), Manager :: pid()) -> {ok, Pid :: pid()} | ignore | {error, Error :: term()}.

start_link(ListenSocket, Manager) ->
    gen_server:start_link(?MODULE, [ListenSocket, Manager], []).

-spec start(ListenSocket :: term(), Manager :: pid()) -> {ok, Pid :: pid()} | ignore | {error, Error :: term()}.

start(ListenSocket, Manager) ->
    gen_server:start(?MODULE, [ListenSocket, Manager], []).

write(Con, Data) ->
    gen_server:call(Con, {write, Data}, infinity).

close(Con, Reason) ->
    gen_server:cast(Con, {close, Reason}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) ->
                    {ok, State :: #s{}} |
                    {ok, State :: #s{}, Timeout :: non_neg_integer()} |
                    ignore |
                    {stop, Reason :: term()}.

init([ListenSocket, Manager]) ->
    erlang:process_flag(trap_exit, true),
    ManMon = erlang:monitor(process, Manager),
    gen_server:cast(self(), start_accept),
    {ok, #s{state = accept, listen_socket = ListenSocket, man_mon = ManMon, manager = Manager}}.

-spec handle_call(Request :: term(), From :: term(), State :: #s{}) ->
                                   {reply, Reply :: term(), State :: #s{}} |
                                   {reply, Reply :: term(), State :: #s{}, Timeout :: non_neg_integer()} |
                                   {noreply, State :: #s{}} |
                                   {noreply, State :: #s{}, Timeout :: non_neg_integer()} |
                                   {stop, Reason :: term(), Reply :: term(), State :: #s{}} |
                                   {stop, Reason :: term(), State :: #s{}}.

handle_call({write, Data}, _From, State = #s{socket = Socket}) ->
    {reply, gen_tcp:send(Socket, Data), State};

handle_call(_Request, _From, State) ->
    ?INT_ERR("Unhandled call: ~10000p", [_Request]),
    {reply, {error, unknown_request}, State}.

-spec handle_cast(Msg :: term(), State :: #s{}) ->
                                  {noreply, State :: #s{}} |
                                  {noreply, State :: #s{}, Timeout :: non_neg_integer()} |
                                  {stop, Reason :: term(), State :: #s{}}.



handle_cast(start_accept, State = #s{listen_socket = ListenSocket, man_mon = ManMon, manager = Manager}) ->
    case gen_tcp:accept(ListenSocket, ?reaccept_timeout) of

        {ok, Socket}     ->
            chronica_tcp_server:accepted(self()),
            catch erlang:demonitor(ManMon, [flush]),
            {noreply, State#s{state = waiting_settings, socket = Socket, man_mon = undefined}};

        {error, timeout} ->
            gen_server:cast(self(), start_accept),
            {noreply, State};

        {error, closed}  ->
            Manager ! listen_socket_closed,
            {stop, normal, State};

        {error, Error}   ->
            {stop, {accept_error, Error}, State}
    end;

handle_cast({close, Reason}, State) ->
    {stop, Reason, State};

handle_cast({add_tcp_connection_result, {ok, Rule}}, State = #s{socket = Socket}) ->
    gen_tcp:send(Socket, io_lib:format("~s~n", ["cache is recompiled"])),
    {noreply, State#s{state = sending_data, name = Rule}};

handle_cast({add_tcp_connection_result, {error, Error}}, State = #s{}) ->
    {stop, {add_rule_failure, Error}, State};

handle_cast(_Msg, State) ->
    ?INT_ERR("Unhandled cast: ~10000p", [_Msg]),
    {noreply, State}.

-spec handle_info(Info :: term(), State :: #s{}) ->
                                   {noreply, State :: #s{}} |
                                   {noreply, State :: #s{}, Timeout :: non_neg_integer()} |
                                   {stop, Reason :: term(), State :: #s{}}.

handle_info({tcp, Socket, Data}, State = #s{state = waiting_settings, socket = Socket, assembling_buff = Buf}) ->
    try erlang:binary_to_list(Data) of
        Str when is_list(Str) -> buffer_changed(Buf ++ Str, State)
    catch
        _:_ ->
            ?INT_ERR("Received invalid settings binary: ~w", [Data]),
            {stop, invalid_settings_binary, State}
    end;

handle_info({tcp_closed, Socket}, State = #s{state = waiting_settings, socket = Socket}) ->
    {stop, normal, State};

handle_info({tcp_closed, Socket}, State = #s{name = Rule, state = sending_data, socket = Socket}) ->
    chronica_manager:remove_tcp_connection(Rule, self()),
    {stop, normal, State};

handle_info({tcp_error, Socket, Error}, State = #s{socket = Socket}) ->
    ?INT_ERR("TCP SOCKET ERROR: ~10000p", [Error]),
    {noreply, State};

handle_info({'DOWN', Mon, _, _, _}, State = #s{man_mon = Mon}) ->
    {stop, master_down, State};

handle_info(_Info, State) ->
    ?INT_ERR("Unhandled info: ~10000p", [_Info]),
    {noreply, State}.

handle_message(who, State = #s{socket = Socket}) ->
    gen_tcp:send(Socket, io_lib:format("~s~n", [node()])),
    {noreply, State};

handle_message({start, Name, Mask, Priority, Format}, State = #s{socket = Socket}) ->
    case validate_start_params(Name, Mask, Priority, Format) of
        ok ->
            Self = self(),
            Continuation = fun (Res) -> gen_server:cast(Self, {add_tcp_connection_result, Res}) end,
            chronica_manager:add_tcp_connection(Mask, Priority, Format, self(), Continuation),
            {noreply, State};
        Error2 ->
            gen_tcp:send(Socket, io_lib:format("~10000p", [Error2])),
            {stop, Error2, State}
    end;

handle_message(Cmd, State) ->
    ?INT_ERR("Received unknown message: ~10000p", [Cmd]),
    {noreply, State}.


-spec terminate(Reason :: term(), State :: #s{}) -> ok.
terminate(Reason, _State = #s{socket = Socket}) ->
    case Reason of
        normal   -> ok;
        shutdown -> ok;
        _ -> ?INT_ERR("Connection terminated: ~10000p", [Reason])
    end,
    catch gen_tcp:close(Socket),
    ok.

-spec code_change(OldVsn :: term(), State :: #s{}, Extra :: term) -> {ok, NewState :: #s{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

buffer_changed(Buf, State) ->
    try
        case read_message(Buf) of

            {message, Msgs, NewBuf} ->
                NewState =
                    lists:foldl(
                        fun (Msg, Acc) ->
                            case handle_message(Msg, Acc) of
                                {noreply, NewAcc} -> NewAcc;
                                {stop, _, _} = Stop -> throw({result, Stop})
                            end
                        end, State#s{assembling_buff = NewBuf}, Msgs),
                {noreply, NewState};

            {no_message, NewBuf} ->
                {noreply, State#s{assembling_buff = NewBuf}}

        end
    catch
        throw:{result, Res} -> Res
    end.

read_message(Buf) ->
    case read_message_(Buf, []) of
        {[], NewBuf} -> {no_message, NewBuf};
        {List, NewBuf} -> {message, lists:reverse(List), NewBuf}
    end.

read_message_("who are you" ++ Buf, Res) -> read_message_(strip(Buf), [who|Res]);
read_message_(("start " ++ Buf) = OldBuf, Res) ->
    case re:run(Buf, "^\\s*(?<A>\\S+)\\s+(?<B>\\S+)\\s+(?<C>\\S+)\\s+(?<D>\\S+)\\s*(?<E>.*)$", [{capture, ['A', 'B', 'C', 'D', 'E'], list}]) of
        {match, [Name, Mask, Priority, Format, Rest]} -> read_message_(strip(Rest), [{start, Name, Mask, Priority, Format}|Res]);
        nomatch -> {Res, OldBuf}
    end;
read_message_(Buf, Res) -> {Res, strip(Buf)}.

strip(" " ++ Buf) -> strip(Buf);
strip("\n" ++ Buf) -> strip(Buf);
strip(Buf) -> Buf.

validate_start_params(_Name, _Mask, Priority, Format) ->
    try
        case Priority of
            "debug" -> ok;
            "trace" -> ok;
            "info"  -> ok;
            "warning" -> ok;
            "error" -> ok;
            _ -> throw({result, {invalid_severity, Priority}})
        end,

        case Format of
            "default" -> ok;
            "binary"  -> ok;
            _ -> throw({result, {invalid_format, Format}})
        end,

        ok

    catch
        throw:{result, Res} -> Res
    end.

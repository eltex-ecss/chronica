%%%-------------------------------------------------------------------
%%% @author Timofey Barmin <timofey.barmin@gmail.com>
%%% @copyright (C) 2011-2013, Eltex, Novosibirsk, Russia
%%%-------------------------------------------------------------------
-module(cycled_events).

-behaviour(gen_server).

-export([
            start/1,
            start_link/1,
            start/2,
            start_link/2,
            add_event/2,
            get_events/1,
            get_events/2,
            get_event/2
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start(MaxEvents) ->
    gen_server:start(?MODULE, [MaxEvents], []).

start(Name, MaxEvents) when is_atom(Name) ->
    gen_server:start({local, Name}, ?MODULE, [MaxEvents], []).

start_link(MaxEvents) ->
    gen_server:start_link(?MODULE, [MaxEvents], []).

start_link(Name, MaxEvents) ->
    gen_server:start_link({local, Name}, ?MODULE, [MaxEvents], []).

add_event(Buffer, Event) ->
    gen_server:cast(Buffer, {add_event, {os:timestamp(), Event}}).

get_events(Buffer) ->
    gen_server:call(Buffer, {get_events, -1}).

get_events(Buffer, Max) ->
    gen_server:call(Buffer, {get_events, Max}).

get_event(Buffer, Id) ->
    gen_server:call(Buffer, {get_event, Id}).

-record(s, {
        max_events :: non_neg_integer(),
        events = queue:new(),
        num = 0 :: non_neg_integer(),
        current_id = 0 :: non_neg_integer()
    }).

-spec init([non_neg_integer()]) -> {ok, #s{}}.
init([MaxEvents]) ->
    {ok, #s{max_events = MaxEvents}}.

handle_cast({add_event, {Time, Event}}, State = #s{num = Num, events = Events,
        max_events = MaxEvents, current_id = Id}) when Num >= MaxEvents ->
    {noreply, State#s{events = queue:in({Id, Time, Event},
                queue:drop(Events)), current_id = Id + 1}};

handle_cast({add_event, {Time, Event}}, State = #s{num = Num, events = Events,
        max_events = MaxEvents, current_id = Id}) when Num < MaxEvents ->
    {noreply, State#s{events = queue:in({Id, Time, Event}, Events),
            num = Num + 1, current_id = Id + 1}};

handle_cast(Unknown, State) ->
    io:format("cycled_events: Unhandled cast request ~p, "
        "state ~1000000p~n", [Unknown, State]),
    {noreply, State}.

handle_call({get_event, Id}, _From, State = #s{current_id = CurId})
when (CurId =< Id) or (Id < 0) ->
    {reply, {error, not_found}, State};

handle_call({get_event, Id}, _From, State = #s{events = Events})  ->
    Q = queue:filter(
        fun ({I, _, _}) when I == Id -> true;
            (_) -> false
        end, Events),
    case queue:to_list(Q) of
        [] -> {reply, {error, not_found}, State};
        [M] -> {reply, {ok, M}, State}
    end;

handle_call({get_events, -1}, _From,
        State = #s{events = Events, num = Num, current_id = Id}) ->
    {reply, {Num, Id, queue:to_list(Events)}, State};

handle_call({get_events, Max}, _From,
        State = #s{events = Events, num = Num, current_id = Id}) when Max >= Num ->
    {reply, {Num, Id, queue:to_list(Events)}, State};

handle_call({get_events, Max}, _From,
        State = #s{events = Events, num = Num, current_id = Id}) when Max < Num ->
    {_, Q} = queue:split(Num - Max, Events),
    {reply, {Max, Id, queue:to_list(Q)}, State};

handle_call(Msg, _From, State) ->
    io:format("cycled_events: Unhandled call request ~10000p~n", [Msg]),
    {reply, {error, bad_msg}, State}.

handle_info(Info, State = #s{}) ->
    io:format("cycled_events: Unhandled info request ~10000p~n", [Info]),
    {noreply, State}.

terminate(Reason, #s{}) ->
    io:format("cycled_events: Terminate becauce of ~10000p~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

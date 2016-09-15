%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_gen_backend).

-behaviour(gen_server).

-include("chronica_int.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").

-export([open/3, close/2, write/2, clear/1, check/1, rotate/1]).

-export(
   [
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

-export_type(
   [
    chronica_data/0
   ]).

-type chronica_data() :: binary() | [byte()].

-callback handle_open(_Param, Options :: [proplists:property()], _Files) ->
    {ok, Handle :: module(), _NewFiles} | {error, _Reason}.

-callback handle_close(Handler :: module()) ->
    ok | {error, _Reason}.

-callback handle_write(Handler :: module(), Data :: chronica_data(),
                       TypeFormat :: binary | text | undefined) ->
    ok | {error, _Reason}.

-callback handle_clear(Handler :: module()) ->
    ok | {error, _Reason}.

-callback handle_check(Handler :: module() | binary()) ->
    boolean().

-callback handle_rotate(Handler :: module()) ->
    ok | {error, _Reason}.


-type outputHandle() :: term().

-spec open(atom(), outputHandle(), [{atom(), term()}]) -> {ok, _Handle} | {error, term()}.
open(Output, Param, Options) ->
    try
        ensure_started(),
        gen_server:call(?MODULE, {open, Output, Param, Options}, infinity)
    catch
        _:E -> {error, {E, erlang:get_stacktrace()}}
    end.

-spec write({atom(), outputHandle()}, {string(), binary | default}) -> ok | {error, term()}.
write({Output, Handle}, {Str, TypeFormat}) ->
    try
        Output:handle_write(Handle, Str, TypeFormat)
    catch
        _:E -> {error, {E, erlang:get_stacktrace()}}
    end.

-spec close({atom(), outputHandle()}, integer()) -> ok | {error, term()}.

close({Output, Handle}, Timeout) ->
    try
        ?INT_DBG("close output: ~p ~p~n", [Output, Handle]),
        gen_server:call(?MODULE, {close, Output, Handle, Timeout})
    catch
        _:E -> {error, {E, erlang:get_stacktrace()}}
    end.

-spec clear({atom(), outputHandle()}) -> ok | {error, term()}.

clear({Output, Handle}) ->
    try
        Output:handle_clear(Handle)
    catch
        _:E -> {error, {E, erlang:get_stacktrace()}}
    end.

-spec check({atom(), outputHandle()}) -> ok | {error, term()}.

check({Output, Handle}) ->
    try
        Output:handle_check(Handle)
    catch
        _:E -> {error, {E, erlang:get_stacktrace()}}
    end.

-spec rotate({atom(), outputHandle()}) -> ok | {error, term()}.

rotate({Output, Handle}) ->
    try
        Output:handle_rotate(Handle)
    catch
        _:E -> {error, {E, erlang:get_stacktrace()}}
    end.

%********************************************************************

-record(s, {closing_flows, files}).

ensure_started() ->
    case erlang:whereis(?MODULE) of
        undefined -> catch gen_server:start_link({local, ?MODULE}, ?MODULE, [], []);
        _ -> ok
    end.

init([]) ->
    erlang:process_flag(trap_exit, true),
    {ok, #s{closing_flows = [], files = sets:new()}}.

handle_cast(_Unknown, State) ->
    ?INT_ERR("Unhandled cast request ~p", [_Unknown]),
    {noreply, State}.

handle_call({open, Output, Param, Options}, _From,
            State = #s{closing_flows = ClosingFlows, files = Files}) ->
    try
        case Output:handle_open(Param, Options, Files) of
            {ok, Handle, NewFiles} ->
                NewClosingFlows = lists:delete({Output, Handle}, ClosingFlows),
                {reply, {ok, {Output, Handle}},
                 State#s{closing_flows = NewClosingFlows,
                         files = NewFiles}};
            Error -> {reply, Error, State}
        end
    catch
        _:E -> {reply, {error, {E, erlang:get_stacktrace()}}, State}
    end;

handle_call({close, Output, Handle, Timeout}, _From, State = #s{files = Files}) when Timeout =< 0 ->
    try
        ?INT_DBG("Closing output: ~100000p/~100000p~n", [Output, Handle]),
        {reply, Output:handle_close(Handle), State#s{files = sets:del_element({Output, Handle}, Files)}}
    catch
        _:E -> {reply, {error, {E, erlang:get_stacktrace()}}, State}
    end;
handle_call({close, Output, Handle, Timeout}, _From, State = #s{closing_flows = ClosingFlows}) when Timeout > 0 ->
    try
        erlang:start_timer(Timeout, self(), {close_timeout, Output, Handle}),
        {reply, ok, State#s{closing_flows = [{Output, Handle}|ClosingFlows]}}
    catch
        _:E -> {reply, {error, {E, erlang:get_stacktrace()}}, State}
    end;

handle_call(_Msg, _From, State) ->
    ?INT_ERR("Unhandled call request~p", [_Msg]),
    {noreply, State}.

handle_info({timeout, _Ref, {close_timeout, Output, Handle}},
            State = #s{closing_flows = ClosingFlows, files = Files}) ->
    NewState = case lists:member({Output, Handle}, ClosingFlows) of
        true  ->
            ?INT_DBG("Closing output after timeout: ~100000p/~100000p~n", [Output, Handle]),
            Output:handle_close(Handle),
            State#s{closing_flows = lists:delete({Output, Handle}, ClosingFlows), files = sets:del_element({Output, Handle}, Files)};
        false ->
            ?INT_DBG("Ignoring close of: ~100000p/~100000p~n", [Output, Handle]),
            State
    end,
    {noreply, NewState};

% TODO: these handlers should be moved to concrete backends...
handle_info({chronica_disk_log, _Node, _Log, {truncated, _N}}, State) ->
    {noreply, State};

handle_info({chronica_disk_log, _Node, _Log, {wrap, _N}}, State) ->
    {noreply, State};

handle_info({chronica_disk_log, _Node, _Log, {error_status, {error, einval}}}, State) ->
    {noreply, State};

handle_info({chronica_disk_log, _Node, _Log, {error_status, {error, {file_error, _, einval}}}}, State) ->
    {noreply, State};

handle_info({chronica_disk_log, _Node, _Log, {error_status, ok}}, State) ->
    {noreply, State};

handle_info({chronica_disk_log, _Node, _Log, {error_status, {error, {file_error, _, enospc}}}}, State) ->
    handle_no_spc(State);

handle_info({chronica_disk_log, _Node, _Log, {error_status, {error, enospc}}}, State) ->
    handle_no_spc(State);

handle_info({disk_log, _Node, _Log, {truncated, _N}}, State) ->
    {noreply, State};

handle_info({disk_log, _Node, _Log, {wrap, _N}}, State) ->
    {noreply, State};

handle_info({disk_log, _Node, _Log, {error_status, {error, einval}}}, State) ->
    {noreply, State};

handle_info({disk_log, _Node, _Log, {error_status, {error, {file_error, _, einval}}}}, State) ->
    {noreply, State};

handle_info({disk_log, _Node, _Log, {error_status, ok}}, State) ->
    {noreply, State};

handle_info({disk_log, _Node, _Log, {error_status, {error, {file_error, _, enospc}}}}, State) ->
    handle_no_spc(State);

handle_info({disk_log, _Node, _Log, {error_status, {error, enospc}}}, State) ->
    handle_no_spc(State);

handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    ?INT_ERR("Unhandled info ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State = #s{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_no_spc(State = #s{files = Files}) ->
    io:format("No free space on disk!!!~n", []),
    lists:map(
        fun ({Output, H}) ->
            Output:handle_close(H)
        end, sets:to_list(Files)),
    {noreply, State#s{files = sets:new()}}.

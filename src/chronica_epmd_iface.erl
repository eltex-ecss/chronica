%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_epmd_iface).
-export([alive2/4]).

-define(epmd_host, "localhost").
-define(epmd_port, 4369).

-define(alive2_req,  120).
-define(alive2_resp, 121).
-define(node_normal, 77).
-define(node_hidden, 72).

%%%===================================================================
%%% API
%%%===================================================================

alive2(Port, NodeType, Node, Timeout) ->
    case gen_tcp:connect(?epmd_host, ?epmd_port, [binary, {packet, 0}, {active, false}], Timeout) of

        {ok, Socket} ->
            case gen_tcp:send(Socket, alive2_req(Port, NodeType, Node)) of
                ok ->

                    case gen_tcp:recv(Socket, 4, Timeout) of
                        {ok, <<?alive2_resp:8, 0:8, _:16>>} -> {ok, Socket};
                        {ok, Data} ->
                            catch gen_tcp:close(Socket),
                            {error, {bad_response, Data}};
                        {error, Error3} ->
                            catch gen_tcp:close(Socket),
                            {error, Error3}
                    end;

                {error, Error2} ->
                    catch gen_tcp:close(Socket),
                    {error, Error2}
            end;

        {error, Error} -> {error, Error}
    end.

alive2_req(Port, NodeType, Node) ->
    NodeType2 =
        case NodeType of
            normal -> ?node_normal;
            hidden -> ?node_hidden;
            _ when is_integer(NodeType) -> NodeType
        end,

    NodeNameStr =
        case Node of
            _ when is_atom(Node) -> erlang:atom_to_list(Node);
            _ when is_list(Node) -> Node
        end,

    NodeNameLen = erlang:length(NodeNameStr),
    NodeBin = erlang:list_to_binary(NodeNameStr),
    Length = 13 + NodeNameLen,
    <<Length:16, 120:8, Port:16, NodeType2:8, 0:8, 5:16, 5:16, NodeNameLen:16, NodeBin/binary, 0:16>>.
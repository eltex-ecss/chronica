%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Anton N Ryabkov
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%% Created : 13 Feb 2013
%%%-------------------------------------------------------------------
-module(chronica_cfg_merge).

-export([
         merge/2
        ]).

-spec merge(Global :: [{atom(), term()}], Node :: [{atom(), term()}]) ->
    {ok, Result :: [{Key :: atom(), Value :: term()}]} | {error, Reason :: term()}.

merge(GlobalCfg, NodeCfg) ->
    MergeCfg = lists:foldl(fun(Item, MergedCfg) -> merge_item(Item, MergedCfg) end, GlobalCfg, NodeCfg),
    {ok, MergeCfg}.

merge_item({Key, CurrentValue}, MergedCfg) when Key =:= rules orelse
                                                Key =:= flows orelse
                                                Key =:= formats orelse
                                                Key =:= internal_logger ->
    OldValue = case lists:keyfind(Key, 1, MergedCfg) of
            {_, OldValue1} -> OldValue1;
            _ -> []
        end,
    NewValue = lists:ukeymerge(1, lists:ukeysort(1, CurrentValue), lists:ukeysort(1, OldValue)),
    lists:keystore(Key, 1, MergedCfg, {Key, NewValue});

% active, rotate_at_start, log_root, max_file_size, max_file_num
merge_item({Key, _Value} = Item, MergedCfg) ->
    lists:keystore(Key, 1, MergedCfg, Item).

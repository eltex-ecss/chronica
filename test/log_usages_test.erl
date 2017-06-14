%%%----------------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Andrew Teplyashin andrew.teplyashin@gmail.com
%%% @copyright (C) 2015, Eltex
%%% @doc
%%% Tests for proof pt_chronica parce_transform worked.
%%% @end
%%%----------------------------------------------------------------------------
-module(log_usages_test).

-ifdef(TEST).

-include("chronica.hrl").
-export([
    module_log_without_args_test/0,
    module_log_with_args_test/0,
    module_log_with_unicode_test/0,
    module_with_todo_test/0,
    module_log_with_env_depend_args_test/0
]).

module_log_without_args_test() ->
    log:debug("Test string!"),
    log:trace("Test string!"),
    log:info("Test string!"),
    log:warning("Test string!"),
    log:error("Test string!").

module_log_with_args_test() ->
    log:debug("Test string with empty args", []),
    log:trace("Test string with empty args", []),
    log:info("Test string with empty args", []),
    log:warning("Test string with empty args", []),
    log:error("Test string with empty args", []),

    log:debug("Test string with arg ~p", [arg]),
    log:trace("Test string with arg ~p", [arg]),
    log:info("Test string with arg ~p", [arg]),
    log:warning("Test string with arg ~p", [arg]),
    log:error("Test string with arg ~p", [arg]),

    Format = string:copies("~p, ", 10),
    _Arg = [ X || X <- lists:seq(1, 10) ],
    log:debug("Test string with external arg " ++ Format, _Arg),
    log:trace("Test string with external arg " ++ Format, _Arg),
    log:info("Test string with external arg " ++ Format, _Arg),
    log:warning("Test string with external arg " ++ Format, _Arg),
    log:error("Test string with external arg " ++ Format, _Arg).

module_log_with_unicode_test() ->
    log:debug("Test string with unicode ☎☢☯", []),
    log:trace("Test string with unicode ☎☢☯", []),
    log:info("Test string with unicode ☎☢☯", []),
    log:warning("Test string with unicode ☎☢☯", []),
    log:error("Test string with unicode ☎☢☯", []).

module_with_todo_test() ->
    log:todo("Test todo"),
    log:todo("Uncorrect todo: ~p", [todo]).

module_log_with_env_depend_args_test() ->
    log:debug("Stack: %stack ~p", [erlang:get_stacktrace()]),
    log:debug("Self pid: %pid ~p", [erlang:self()]).

-endif. %% TEST

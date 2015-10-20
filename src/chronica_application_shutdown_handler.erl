%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Anton N Ryabkov, Teplyashin Andrey
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%     Handler for logging node crash reason in system log.
%%% @end
%%% Created : 15 May 2014
%%%-------------------------------------------------------------------
-module(chronica_application_shutdown_handler).

-export([
         handle_shutdown/1
        ]).

handle_shutdown(Reason) ->
    try
        ReasonStr = exception_format(parse_term(Reason)),
        case os:type() of
            {unix, _} ->
                Cmd = lists:flatten(io_lib:format("logger -t ~p[~p] Node stopped by "
                    "reason: ~p", [node(), os:getpid(), ReasonStr])),
                os:cmd(Cmd);
            {win32, _} ->
                Cmd = io_lib:format("EVENTCREATE /T ERROR /ID 999 /L "
                    "APPLICATION /D \"Node ~p[~p] stoped by reason: ~p\"",
                    [node(), os:getpid(), ReasonStr]),
                os:cmd(Cmd);
            {OsFamily, _} ->
                io:format("Unsupported OS: ~p~n. "
                          "Node stoped by reason: ~s~n", [OsFamily, ReasonStr])
        end
    catch
        throw:not_logged_it ->
            ok;
        _:_ ->
            io:format("Node stoped by reason: ~s~n", [Reason])
    end.

exception_format({application_start_failure, App, Reason}) ->
    exception_app_format(App, Reason);
exception_format(Reason) -> Reason.

exception_app_format(kernel, {{shutdown,
    {failed_to_start_child, net_sup, {shutdown,
        {failed_to_start_child, net_kernel, {'EXIT', nodistribution}}}}},
                              {kernel, start, [normal, []]}}) ->
    throw(not_logged_it);
exception_app_format(chronica, {{bad_return, {chronica_supervisor, init, Reason}}, _Stack}) ->
    exception_chronica_format(Reason);
exception_app_format(AppName, {{shutdown, {failed_to_start_child, chronica_manager,
        {error, {parse_flow, bad_format_type}}}}, _Stack}) ->
    io_lib:format("application ~p can't start cause one of the flows has invalid flow format.", [AppName]);
exception_app_format(AppName, {{shutdown, {failed_to_start_child, chronica_manager,
        {error, {parse_flow, cant_open_output}}}}, _Stack}) ->
    io_lib:format("application ~p can't start cause one of the flows can't "
                  "open output stream (file, tty, tcp, etc.)", [AppName]);
exception_app_format(AppName, Error) ->
    io_lib:format("application ~p can't start by reason: ~100000p", [AppName, Error]).

exception_chronica_format({ensure_dir_error, {Path, {error, eacces}}}) ->
    io_lib:format("chronica application can't start cause no write access to the directory ~p", [Path]);
exception_chronica_format({ensure_dir_error, {Path, {error, edquot}}}) ->
    io_lib:format("chronica application can't start cause disk quota exceeded to the directory ~p", [Path]);
exception_chronica_format({ensure_dir_error, {Path, {error, enfile}}}) ->
    io_lib:format("chronica application can't start cause file table overflow to the directory ~p", [Path]);
exception_chronica_format({ensure_dir_error, {Path, {error, enomem}}}) ->
    io_lib:format("chronica application can't start cause not enough memory to the directory ~p", [Path]);
exception_chronica_format({ensure_dir_error, {Path, {error, enospc}}}) ->
    io_lib:format("chronica application can't start cause no space left on device to the directory ~p", [Path]);
exception_chronica_format({ensure_dir_error, {Path, {error, enotdir}}}) ->
    io_lib:format("chronica application can't start cause not a directory the directory ~p", [Path]);
exception_chronica_format({open_file_error, {error, {file_error, File, eacces}}}) ->
    io_lib:format("chronica application can't start cause no write access to the directory ~p", [File]);
exception_chronica_format({open_file_error, {error, {file_error, File, edquot}}}) ->
    io_lib:format("chronica application can't start cause disk quota exceeded to the directory ~p", [File]);
exception_chronica_format({open_file_error, {error, {file_error, File, enfile}}}) ->
    io_lib:format("chronica application can't start cause file table overflow to the directory ~p", [File]);
exception_chronica_format({open_file_error, {error, {file_error, File, enomem}}}) ->
    io_lib:format("chronica application can't start cause not enough memory to the directory ~p", [File]);
exception_chronica_format({open_file_error, {error, {file_error, File, enospc}}}) ->
    io_lib:format("chronica application can't start cause no space left on device to the directory ~p", [File]);
exception_chronica_format({open_file_error, {error, {file_error, File, enotdir}}}) ->
    io_lib:format("chronica application can't start cause not a directory the directory ~p", [File]).

parse_term(StrTerm) when is_list(StrTerm) ->
    StrScanTerm = case string:right(StrTerm, 1) of
        "." -> StrTerm;
        _   -> StrTerm ++ "."
    end,
    case erl_scan:string(StrScanTerm) of
        {ok, Tokens, _EndLocation} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} ->     Term;
                {error, Error} -> {error, {parse_term, Error}}
            end;
        {error, ErrorInfo, _EndLocation} ->
            {error, {parse_term, ErrorInfo}}
    end;
parse_term(Args) ->
    {error, {not_string, Args}}.

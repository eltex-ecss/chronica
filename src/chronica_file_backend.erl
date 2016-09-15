%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%     File backend using a little bit modified disk_log
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_file_backend).

-include_lib("chronica_int.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").

-behaviour(chronica_gen_backend).

-export(
   [
    handle_open/3,
    handle_close/1,
    handle_write/3,
    handle_clear/1,
    handle_rotate/1,
    handle_check/1,
    get_header/0,
    get_application_info/1,
    get_application_info/0
   ]).

handle_open(Filename, Options, Files) ->
    case handle_open_(Filename, Options) of
        {ok, Handle} ->
            {ok, Handle, sets:add_element({?MODULE, Handle}, Files)};
        Other ->
            Other
    end.

handle_open_(Filename, Options) ->
    ?INT_DBG("creating log ~p~n", [Filename]),
    try
        LogRoot = proplists:get_value(log_root, Options, undefined),
        MaxSize = proplists:get_value(max_file_size, Options, undefined),
        MaxNum  = proplists:get_value(max_file_num, Options, undefined),

        case (MaxNum == undefined) or (MaxSize == undefined) or (LogRoot == undefined) of
            false -> ok;
            true  -> throw({invalid_options, Options})
        end,

        case is_list(Filename) of
            true ->
                CheckedName = check_filename(Filename, LogRoot),
                LogName = erlang:list_to_atom("chronica_file_backend_" ++ Filename),
                case lists:member(LogName, element(1, chronica_disk_log:accessible_logs())) of
                    false ->
                        Res = case chronica_disk_log:open([
                                            {name, LogName},
                                            {file, CheckedName},
                                            {format, external},
                                            {type, wrap},
                                            {size, {MaxSize, MaxNum}},
                                            {notify, true}
                                            ]) of
                            {ok, Handler} ->
                                {ok, Handler};
                            {error, {name_already_open, Handler}} ->
                                {ok, Handler};
                            {repaired, Handler, _, _} ->
                                {ok, Handler};
                            {error, {size_mismatch, Cursize, NewSize} = Reason} ->
                                ?INT_DBG("Size of ~p changed ~p -> ~p", [CheckedName, Cursize, NewSize]),
                                case chronica_disk_log:open([{name, LogName}, {file, CheckedName}, {format, external}, {type, wrap}, {size, Cursize}]) of
                                            {Succ, Handler} when Succ =:= ok;
                                                                 Succ =:= name_already_open;
                                                                 Succ =:= repaired ->
                                                case chronica_disk_log:change_size(Handler, NewSize) of
                                                    ok ->
                                                        ?INT_DBG("File ~p successfully open", [CheckedName]),
                                                        %io:format("Log created ~p~n", [Handler]),

                                                        {ok, Handler};
                                                    {error, Reason3} ->
                                                        ?INT_ERR("File ~p successfully open, but cant change size(~p->~p) cause: ~p~n", [CheckedName, Cursize, NewSize, Reason3]),
                                                        %io:format("Log created ~p~n", [Handler]),
                                                        {ok, Handler}
                                                end;
                                            {error, Reason2} -> {error, {Reason, Reason2}};
                                            Err ->
                                                ?INT_ERR("internal_logger:init_file() unhandled result of open ~p~n", [Err]), Err
                                        end;
                            Error ->
                                ?INT_ERR("Error open file ~p", [Error]),
                                Error
                        end,
                        Res;
                    true ->
                        {ok, LogName}
                end;
            false ->
                ?INT_ERR("Filename should be a string: ~p", [Filename]),
                {error, bad_filename}
        end
    catch
        _C:E ->
            {error, E}
    end.

handle_close(Handler) ->
    try
        chronica_disk_log:close(Handler)
    catch
        _C:E ->
            {error, E}
    end.

handle_write(Handler, Data, TypeFormat) when TypeFormat =:= binary ->
    try
        case chronica_disk_log:balog(Handler, chronica_format_creator:frame(Data)) of
            {error, Reason} ->
                {error, Reason};
            ok ->
                ok
        end
    catch
        _C:E ->
            {error, E}
    end;

handle_write(Handler, Str, _TypeFormat) ->
    try
        case chronica_disk_log:balog(Handler, Str) of
            {error, Reason} ->
                {error, Reason};
            ok -> ok
        end
    catch
        _C:E ->
            {error, E}
    end.

handle_clear(Handle) ->
    try
        chronica_disk_log:btruncate(Handle, <<"">>)
    catch
        _C:E ->
            {error, E}
    end.

handle_rotate(Handle) ->
    chronica_disk_log:inc_wrap_file(Handle).

handle_check(Handle) ->
    case chronica_disk_log:info(Handle) of
        {error, no_such_log} ->
            false;
        {error, einval} ->
            false;
        _ ->
            true
    end.

check_filename(Name, LogRoot) ->
    TmpName = case lists:last(Name) of
                $/ ->
                    Name ++ ?LOG_FILE_DEFAULT;
                _ ->
                    Name
              end,
    ?INT_DBG("Check name ~p", [TmpName]),
    case filelib:ensure_dir(Checked = normalize(LogRoot ++ "/" ++ TmpName)) of
        ok ->
            ?INT_DBG("Checked ~p", [Checked]),
            Checked;
        {error, Error} ->
            throw({Checked, Error})
    end.


normalize(Name) ->
    Filename = filename:basename(Name),
    ["/" | AbsDirs] = filename:split(filename:absname(filename:dirname(Name))),
    ?INT_DBG("dirs ~p", [AbsDirs]),
    Dirs = proc(delete_dots(AbsDirs), []),
    ?INT_DBG("dirs after proc ~p", [Dirs]),
    filename:absname("/" ++ string:join(Dirs, "/") ++ "/" ++ Filename).

delete_dots(["." | Tail]) -> delete_dots(Tail);
delete_dots([Dir | Tail]) -> [Dir | delete_dots(Tail)];
delete_dots([]) -> [].

proc([_Dir | [ ".." | Tail]], ClearPath) -> proc(ClearPath ++ Tail, []);
proc([ Dir | Tail], ClearPath) -> proc(Tail, ClearPath ++ [Dir]);
proc([], ClearPath) -> ClearPath.

get_header() ->
    Str =
        try
            get_application_info()
        catch
            _:_ -> "build_info is unavailable\n"
        end,
    {ok, Str}.


get_application_info() ->
    try
        Apps = application:loaded_applications(),
        R = lists:foldl(
            fun ({AppName, _, _}, Acc) ->
                [get_application_info(AppName) | Acc]
            end, [], Apps),
        string:join(R, "\n") ++ ["\n\n"]
    catch
        _:E -> lists:flatten(io_lib:format("Cant get application info cause ~p", [E]))
    end.

get_application_info(App) ->
    try
        {AppName, AppDesc, _Version} = lists:keyfind(App, 1, application:loaded_applications()),
        {ok, Version} = application:get_key(AppName, vsn),
        BI = get_application_build_info(AppName),
        Strings = [ [$\s, $\s, $\s |S] || S <- format_build_info(BI, [])],
        string:join(
            [lists:flatten(io_lib:format("Application: ~p(~s) ~p", [AppName, AppDesc, Version])) | Strings], "\n")
    catch
        _:E -> lists:flatten(io_lib:format("Application ~p: no info (~p, ~p, ~p)", [App, E, application:which_applications(), erlang:get_stacktrace()]))
    end.


get_application_build_info(AppName) ->
    try
        {ok, Keys} = application:get_all_key(AppName),
        {_, Modules = [_|_]} = lists:keyfind(modules, 1, Keys),
        case find_latest_build_info(Modules, 0) of
            0 -> [];
            Module -> Module:build_info()
        end
    catch
        _:_E -> [{error, lists:flatten(io_lib:format("can't get build info for application ~p cause ~p", [AppName, _E]))}]
    end.

find_latest_build_info([], Res) -> Res;
find_latest_build_info([Module | Tail], 0) ->
    Res =
        try
            Module:build_info(),
            Module
        catch
            _:_ -> 0
        end,
    find_latest_build_info(Tail, Res);
find_latest_build_info([Module | Tail], Res) ->
    try
        BI = Module:build_info(),
        case lists:keyfind(build_date, 1, BI) of
            false -> find_latest_build_info(Tail, Res);
            {_, Now1} ->
                {_, Now2} = lists:keyfind(build_date, 1, Res:build_info()),

                if (Now1 > Now2) ->
                    find_latest_build_info(Tail, Module);
                   true ->
                    find_latest_build_info(Tail, Res)
                end
        end
    catch
        _:_E -> find_latest_build_info(Tail, Res)
    end.

format_build_info([], Res) -> lists:reverse(Res);
format_build_info([{build_date, Now = {_, _, _}}| Tail], Res) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:now_to_local_time(Now),
    format_build_info(Tail, [ lists:flatten(io_lib:format("Build date ~2.10.0B.~2.10.0B.~4.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [D, M, Y, H, Mi, S])) |Res]);
format_build_info([{build_date, Now}| Tail], Res) ->
    format_build_info(Tail, [ lists:flatten(io_lib:format("Build date ~s", [Now])) |Res]);
format_build_info([{build_author, Author} | Tail], Res) ->
    format_build_info(Tail, [ lists:flatten(io_lib:format("Build by ~p", [Author])) | Res]);
format_build_info([{build_host, Host} | Tail], Res) ->
    format_build_info(Tail, [ lists:flatten(io_lib:format("Build on host ~p", [Host])) | Res]);
format_build_info([{commit, Id} | Tail], Res) ->
    format_build_info(Tail, [ lists:flatten(io_lib:format("Last commit ~p", [Id])) | Res]);
format_build_info([{commit_author, Author} | Tail], Res) ->
    format_build_info(Tail, [ lists:flatten(io_lib:format("Committed by ~p", [Author])) | Res]);
format_build_info([{commit_date, Date} | Tail], Res) ->
    format_build_info(Tail, [ lists:flatten(io_lib:format("Commited at ~p", [Date])) | Res]);
format_build_info([{Key, Value} | Tail], Res) ->
    format_build_info(Tail, [ lists:flatten(io_lib:format("~p: ~p", [Key, Value])) | Res]);
format_build_info([P | Tail], Res) ->
    format_build_info(Tail, [ lists:flatten(io_lib:format("~p", [P])) | Res]).

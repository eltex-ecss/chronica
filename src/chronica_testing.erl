%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Timofey Barmin
%%% @copyright (C) 2015, Eltex, Novosibirsk, Russia
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chronica_testing).
-export([auto/0, load_test/3, profiling_test/1]).
-export([idcalc/1, time/1, do/2, do_format_test/1, ttt/0, test_funtrace/2]).

-include_lib("pt_lib/include/pt_lib.hrl").
-include("chronica.hrl").
-include_lib("pt_scripts/include/pt_recompilable.hrl").
-include_lib("pt_scripts/include/pt_versioned.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").

-compile({fun_trace, exported}).
-compile({fun_trace, [test_funtrace/2]}).
-compile({fun_trace_opt, leave}).

-record(test_record, {a, b, c}).
-record(test_record2, {a, b, c}).

ttt() ->
    log:error("~p ~B ~w ~n ~X", [a, 2, c, 3, 5]).

time(Fun) ->
    StartSecs = erlang:system_time(seconds),
    Fun(),
    StopSecs = erlang:system_time(seconds),
    io:format("Finished: ~bs ~n", [StopSecs - StartSecs]).

do(0, _) -> ok;
do(N, Fun) ->
    Fun(),
    do(N - 1, Fun).

do_format_test(N) ->
    {match, _} = re:run("ssssXXXddd", ?REGEXP(".*")),
    P1 = {flows,
           [
                {flow1, [{file, "./logs/.././llll/../321/log1", detailed}]},
                {flow2, [{file, "log2.log"}]},
                {flow3, [tty, {file, "flow3"}]},
                {flow4, [{file, "log2.log"}, {file, "./log1"}, tty, {file, "./folder/../log1"}, tty, {file, "log1"}, {file, "log2"}]},
                {disk_log, [tty, {file, "disk_log"}]},
                {error_logger, [{file, "error_logger"}, {udp, {"192.168.1.14", 2001}}]},
                {default, [{file, "./"}]}
           ]
         },
    P2 = {flows,
           [
                {flow1, [{file, "./logs/.././llll/../321/log1", detailed}]},
                {flow3, [{file, "log2.log"}]},
                {flow4, [tty, {file, "flow3"}]},
                {flow6, [{file, "log2.log"}, {file, "./log1"}, tty, {file, "./folder/../log1"}, tty, {file, "log1"}, {file, "log2"}]},
                {disk2_log, [tty, {file, "disk_log"}]},
                {errorg_logger, [{file, "error_logger"}, {udp, {"192.168.1.14", 2001}}]},
                {defaultf, [{sfdile, "./"}]}
           ]
         },
    S1 = "sdajhflkj asdh fasdhf kjghdasf kjdhgasf k agdsh fhdsgaf hasdg "
         "fkjdhasgf kjagsf dfsjg;kl sdlifgydl",
    X = fun () ->
            io_lib:format("sakjdf hjhgdsjhgdshghg sdkh gdksjfh ~p "
                          "sfkjhdkjfhgdshg ~s dsf.lkjdkjfhgkdjfhgdhfg ~n "
                          "fdkljlkjgsdfj g fgd  ~n ldkfjglkdfs g  ~p", [P1, S1, P2])
        end,
    Y = fun () ->
            io_lib:format("sakjdf hjhgdsjhgdshghg sdkh gdksjfh ~w "
                          "sfkjhdkjfhgdshg ~s dsf.lkjdkjfhgkdjfhgdhfg ~n "
                          "fdkljlkjgsdfj g fgd  ~n ldkfjglkdfs g  ~w", [P1, S1, P2])
        end,
    Z = fun () ->
            [
             "sakjdf hjhgdsjhgdshghg sdkh gdksjfh ",
             io_lib_pretty:print(P1),
             " sfkjhdkjfhgdshg ", S1,
             " dsf.lkjdkjfhgkdjfhgdhfg ~n fdkljlkjgsdfj g fgd  ~n "
             "ldkfjglkdfs g ", io_lib_pretty:print(P2)
            ]
        end,
    time(fun() -> do(N, X) end),
    time(fun() -> do(N, Y) end),
    time(fun() -> do(N, Z) end),
    ok.

auto() ->
    do_test("Crash test", fun log_test/1),
    ok.

do_test(Name, TestFun) ->
    io:format(Name , []),

    case (catch run_all_tests(TestFun, ok, all)) of
        ok -> io:format(" ok~n", []), ok;
        Err -> io:format(" failed~n", []), Err
    end.

run_all_tests(TestFun, DefRes, all) ->
    run_all_tests(TestFun, DefRes, 1);
run_all_tests(TestFun, DefRes, N) ->
    Res = case catch TestFun(N) of
            ok -> ok;
            not_supp ->  io:format("Test ~p... not supported~n", [N]), ok;
            not_impl ->  io:format("Test ~p... not implemented~n", [N]), ok;
            _Err -> io:format("Test ~p... failed~n~p~n", [N, _Err]), failed
          end,

    case TestFun(num) of
        N ->
            DefRes;
        _ ->
            run_all_tests(TestFun, case DefRes of ok -> Res; _ -> DefRes end , N + 1)
    end.

load_test(ProcessNum, MessTimeout, Time) ->
    Pid = spawn(fun() ->
        wait_all_process(ProcessNum, erlang:system_time(seconds))
    end),
    start_test(ProcessNum, MessTimeout, Time),
    register(wait_process, Pid).

start_test(0, _, _) -> ok;
start_test(ProcessNum, MessPerSec, Time) ->
    spawn(fun() -> testing_process(trunc(1000 / MessPerSec), MessPerSec * Time) end),
    start_test(ProcessNum - 1, MessPerSec, Time).

testing_process(_, 0) -> wait_process ! {self(), finished}, ok;
testing_process(MessTimeout, MessNum) ->
    receive
    after
         MessTimeout ->
            log:log(2, module, module, 123, "test", []),
            testing_process(MessTimeout, MessNum - 1)
    end.

wait_all_process(0, StartSecs) ->
    StopSecs = erlang:system_time(seconds),
    io:format("Finished: ~bs ~n", [StopSecs - StartSecs]);
wait_all_process(ProcessNum, StartTime) ->
    receive
        {_, finished} ->
            wait_all_process(ProcessNum - 1, StartTime)
    after
        100000 ->
            io:format("Timeout~n")
    end.

idcalc(Str36) ->
    {ok, [Num | _], _} = io_lib:fread("~36u", Str36),
    Num.

log_test(0) -> ok;
log_test(1) -> ok = log:error("error test");
log_test(2) -> ok = log:error("error test:~p ~p", [1, some_atom]);
log_test(3) -> ok = log:error(some_module, "error test:~p ~p", [2, some_atom]);
log_test(4) -> ok = log:warning("warning test");
log_test(5) -> ok = log:warning("warning test:~p ~p", [1, some_atom]);
log_test(6) -> ok = log:warning([some_module, chronica_testing], "warning test:~p ~p", [3, some_atom]);
log_test(7) -> ok = log:info("info test");
log_test(8) -> ok = log:info("info test:~p ~p", [1, some_atom]);
log_test(9) -> ok = log:info(chronica_testing, "info test:~p ~p", [4, some_atom]);
log_test(10) -> ok = log:trace("trace test");
log_test(11) -> ok = log:trace("trace test:~p ~p", [1, some_atom]);
log_test(12) -> ok = log:trace([some_module, some_module3], "trace test:~p ~p", [5, some_atom]);
log_test(13) -> ok = log:debug("dbg test");
log_test(14) -> ok = log:debug("dbg test:~p ~p", [1, some_atom]);
log_test(15) -> ok = log:debug(some_module2, "dbg test:~p ~p", [6, some_atom]);
log_test(num) -> 15.

test_funtrace(aaa,  A = #test_record{a = C} = _S) ->
    {A, C};
test_funtrace(aaa,  _S = {asdas, _, _, _, {dffgs, _, [B | _]}}) ->
    B;
test_funtrace(aaa,  {asdas, _, _, _, {dffg, _, [B, C, _]}}) ->
    {B, C};
test_funtrace(aaa,  A = #test_record2{}) ->
    A;
test_funtrace(aaa, {A = {C, _}, a} = State) ->
    {A, C, State}.

profiling_test(N) ->
    MyPid = self(),
    Pid = spawn(fun () -> bombing_proc(N, MyPid) end),
    eprof:start(),
    EProfPid = whereis(eprof),
    Linked = element(2, lists:keyfind(links, 1, erlang:process_info(whereis(eprof)))),
    Ex = [self(), EProfPid | Linked],
    Ps = [Pid, chronica_core] ++ lists:filter(fun(X) -> is_pid(X) end, erlang:processes()--Ex),
    io:format("Process list: ~p~n", [Ps]),
    eprof:start_profiling(Ps),
    Pid ! start,
    receive
        finished ->
            ok
    end,
    eprof:stop_profiling(),
    eprof:analyze().

bombing_proc(N, From) ->
    receive
        start ->
            io:format("starting bombing~n"),
            bombing(N),
            io:format("bombing finished~n"),
            From ! finished
    end.

bombing(0) -> ok;
bombing(N) ->
    P = self(),
    io_lib:format("~p ~p sfdgdsfjgsdf g~p sfjdskjh gsd~p",
                  [{asfdkjh, dfjhgd},
                   sdjkfsjkdhf,
                   sadkjfhaskjf,
                   [
                    sdfgdsfgdsfg,
                    dfgsdfgsd,
                    gsdfgdsgsd,
                    gsdfgsdfgsdfg,
                    dsfgsdfgdsgdsfgsdfg,
                    df
                   ]]),
    log:debug(?MODULE, "bombing from ~p", [P]),
    bombing(N - 1).

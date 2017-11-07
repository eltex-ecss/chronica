-module(chronica_test_backend).
-export([test/0]).
-include_lib("chronica/include/chronica.hrl").
-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).
-define(RETRY, 10).

test() ->
    log:debug([testing1], "testing_set_equal_rule", []),

    log:debug([testing2], "testing_complex_rule", []),

    log:debug([testing3], "testing_on_off_rule", []),
    chronica_manager:update_rule_inwork(testing_on_off_rule1, false),
    log:debug([testing3], "testing_on_off_rule", []),

    log:debug([testing4], "testing_set_equal_flow", []),

    log:debug([testing5], "testing_set_equal_rule_and_flow", []),

    log:error([testing6], "testing_rising_log", []),
    log:debug([testing6], "testing_rising_log", []),

    check_true_test(),
    init:stop().

check_true_test() ->
    timer:sleep(5000),
    {ok, Cwd} = file:get_cwd(),
    testing1(Cwd),
    testing2(Cwd),
    testing3(Cwd),
    testing4(Cwd),
    testing5(Cwd),
    testing6(Cwd).

testing1(Cwd) ->
    Data_file = get_data_file(Cwd ++ "/log/file_testing1.log"),
    ok = assert(Data_file, "DEBUG testing_set_equal_rule").

testing2(Cwd) ->
    Data_file = get_data_file(Cwd ++ "/log/file_testing2.log"),
    ok = assert(Data_file, "DEBUG testing_complex_rule").

testing3(Cwd) ->
    Data_file = get_data_file(Cwd ++ "/log/file_testing3.log"),
    ok = assert(Data_file, "DEBUG testing_on_off_rule").

testing4(Cwd) ->
    Data_file = get_data_file(Cwd ++ "/log/file_testing4.log"),
    ok = assert(Data_file, "DEBUG testing_set_equal_flow").

testing5(Cwd) ->
    Data_file = get_data_file(Cwd ++ "/log/file_testing5.log"),
    ok = assert(Data_file, "DEBUG testing_set_equal_rule_and_flow").

testing6(Cwd) ->
    Data_file = get_data_file(Cwd ++ "/log/file_testing6.log"),
    ok = assert(Data_file, "ERROR testing_rising_log").

assert([Head | Tail], Final_result) when Final_result == Head, Tail == []->
    ok;
assert(_, _) ->
    erlang:throw({error_tests, "Tests failed or are long executed"}).

get_data_file(Cwd) ->
    {ok, Testing1} = file:read_file(Cwd),
    string:tokens(binary_to_list(Testing1), "\n").

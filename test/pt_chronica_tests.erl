-module(pt_chronica_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

pt_chronica_test_() ->
    [
        ?_test(
            begin
                [
                    'Log0',
                    'Log10',
                    'Log9',
                    'Log0',
                    'Log4',
                    'Log1',
                    'Log10',
                    'Log5',
                    'Log14'
                ] = flag_chronica_optimization_test()
            end
        )
    ].

flag_chronica_optimization_test() ->
    TestAST = pt_lib:str2ast("handle_call(hello_error, From, State) ->
        Log0 = a,
        Log4 = a,
        Log2 = c,
        Log3 = c,
        Log11 = c,
        Log6 = c,
        try
            Log12 = a,
            case Log3 of
                {a, Res1} ->
                    fun(Log1, Log5, Log8) ->
                        Log7 = c,
                        Log21 = a,
                        receive
                            Log9 when Log9 =:= Log21;  Log9 =:= Log6 ->
                                log:error(\"test0 ~p~n\", [{Log2, Log4, Log5, Log0, Log7, Log9, Log21}]),
                                Acc = b,
                                if
                                    Log12 ->
                                        log:error(\"test1 ~p~n\", [
                                            {State, Log1, Acc, Log2, Log3, Log5, Log6, Log8, Log12}
                                        ]);
                                    Acc ->
                                        Acc = b,
                                        Acc
                                end;
                            _ ->
                                Log7 = a,
                                as
                        after
                            123 ->
                                Log9 = a,
                                Log10 = b,
                                log:error(\"test4 ~p~n\", [
                                    {State, Log1, Log2, Log3, Log5, Log6, Log8, Log9, Log10, Log11}
                                ])
                        end
                    end;
                {b, Res2} ->
                    case Log2 of
                        a ->
                            log:error(\"test2 ~p~n\", [{From, Res2, Log2, Log3}]),
                            Log2;
                        Log2 ->
                            Acc = b,
                            Acc
                    end;
                {b, Res3} ->
                    Log1 = b,
                    log:error(\"test3 ~p~n\", [{From, Log1, Res3, Log2, Log3}])
            end,
            Res =
                begin
                    case Log3 of
                        a ->
                            Log11;
                        _ ->
                            b
                    end
                end
        catch
            Log2 ->
                Log5 = a,
                Log10 = a,
                log:error(\"test3 ~p~n\", [{From, Log2, Log3, Log5, Log10, Log6}]);
            _ ->
                Log14 = a,
                log:error(\"test3 ~p~n\", [{From, Log2, Log14}])
        after
            log:error(\"test3 ~p~n\", [{From, Log2, Log0}])
        end,
        {reply, ok, State}.", 1
    ),
    DataStateLog = pt_chronica:return_state_log(TestAST),
    MatchVar =
        fun(StatVar, Acc) ->
            ListDeactiveLog = pt_chronica_optimization:init_match_var([StatVar], []),
            Flatten =
                fun(DeactiveLog, LocAcc) ->
                    maps:keys(DeactiveLog) ++ LocAcc
                end,
            lists:foldl(Flatten, [], ListDeactiveLog) ++ Acc
        end,
    lists:foldl(MatchVar, [], DataStateLog).

-endif. %% TEST

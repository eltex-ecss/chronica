-module(chronica_rule).
-export(
   [
    parse_pattern/1
   ]).

parse_pattern(P) ->
    P1 = parse_brakets(P),
    P2 = parse_binary_op(P1, "|"),
    P3 = parse_binary_op(P2, "&"),
    parse_unary_op(P3, "!").

parse_brakets("(" ++ P) ->
    {SubP, Tail} = split_pair_braket(P),
    [parse_brakets(SubP) | parse_brakets(Tail)];
parse_brakets([C|P]) ->
    [C | parse_brakets(P)];
parse_brakets([]) -> [].

parse_binary_op([{_, _} = Op1], Op) ->
    parse_binary_op(Op1, Op);
parse_binary_op({Op1, {P1, P2}}, Op) ->
    {Op1, {parse_binary_op(P1, Op), parse_binary_op(P2, Op)}};
parse_binary_op({Op1, {P1}}, Op) ->
    {Op1, {parse_binary_op(P1, Op)}};
parse_binary_op(List, Op) when is_list(List) ->
    case string:tokens(List, Op) of
        [First] ->
            lists:map(
                fun ([E]) when is_list(E) -> parse_binary_op(E, Op);
                    (E) -> parse_binary_op(E, Op)
                end, First);
        [First|Tokens] ->
            lists:foldl(
                fun
                    ([E], Cur) when is_list(E) ->
                        {Op, {Cur, parse_binary_op(E, Op)}};
                    (E, Cur) ->
                        {Op, {Cur, parse_binary_op(E, Op)}}
                end, parse_binary_op(First, Op), Tokens)
    end;
parse_binary_op(E, _Op) -> E.

parse_unary_op([{_, _} = Op1], Op) ->
    parse_unary_op(Op1, Op);
parse_unary_op({Op1, {P1, P2}}, Op) ->
    {Op1, {parse_unary_op(P1, Op), parse_unary_op(P2, Op)}};
parse_unary_op({Op1, {P1}}, Op) ->
    {Op1, {parse_unary_op(P1, Op)}};
parse_unary_op(List, Op) when is_list(List) ->
    Str = string:strip(List),
    case lists:prefix(Op, Str) of
        true  -> {Op, {parse_unary_op(Str -- Op, Op)}};
        false -> List
    end.

split_pair_braket(P) ->
    split_pair_braket_(P, [], 0).
split_pair_braket_([], Acc1, _) ->
    throw({no_pair_braket, lists:reverse(Acc1)});
split_pair_braket_(")" ++ Tail, Acc1, 0) ->
    {lists:reverse(Acc1), Tail};
split_pair_braket_(")" ++ Tail, Acc1, BN) when BN > 0 ->
    split_pair_braket_(Tail, [$)|Acc1], BN - 1);
split_pair_braket_(")" ++ Tail, _Acc1, _BN) ->
    throw({invalid_braket, Tail});
split_pair_braket_("(" ++ Tail, Acc1, BN) ->
    split_pair_braket_(Tail, [$(|Acc1], BN + 1);
split_pair_braket_([C|Tail], Acc1, BN) ->
    split_pair_braket_(Tail, [C|Acc1], BN).


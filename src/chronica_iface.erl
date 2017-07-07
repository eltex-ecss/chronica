-module(chronica_iface).

-include("chronica_int.hrl").
-include("chronica_config.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").
-include_lib("pt_lib/include/pt_lib.hrl").

-export(
   [
    generate_app_iface_modules/2,
    get_appropriate_flows/3,
    generate_iface_module/2
   ]).

generate_app_iface_modules(App, Rules) ->
    AppModules =
        case application:get_key(App, modules) of
            {ok, L2} -> L2;
            _ -> []
        end,

    lists:foldl(
        fun (M, Acc) ->
            ?INT_DBG("Generating iface module for ~p", [M]),
            NewAcc =
                case generate_iface_module(M, Rules) of
                    undefined ->
                        ?INT_DBG("not generated", []),
                        Acc;
                    Code      ->
                        ?INT_DBG("generated", []),
                        [{M, Code}|Acc]
                end,
            NewAcc
        end, [], AppModules).

generate_iface_module(ModuleName, Rules) ->
    case (catch ModuleName:get_log_tags()) of
        TagList when is_list(TagList) ->
            timer:sleep(5), % reduce overload
            IfaceName = pt_chronica:generate_module_iface_name(ModuleName),
            AST  = pt_lib:create_clear_ast(IfaceName),

            DefaultClause =
                ast("(Priority, Key, Module, Line, File, Function, Format, Args, Self, Now) ->
                        chronica_core:sync_fast_log({fast_log_message, Priority, Format, Args, Module, Line, File, Function, Self, Now, @IfaceName, Key}).", 0),
            Rules2 = get_appropriate_flows_exclusion(ModuleName, Rules),

            Flows = [
                {Tags2, [{Priority, get_appropriate_flows(Tags2, Priority, Rules2)} || Priority <- [?P_ERROR, ?P_WARNING, ?P_INFO, ?P_TRACE, ?P_DEBUG]]}

                ||

                Tags2 <- TagList
            ],
            Clauses = generate_clauses(TagList, Flows) ++ [DefaultClause],
            AST2 = pt_lib:add_function(AST, ast("log_fast [...$Clauses...].", 0)),

            DefaultClause2 = ast("([], Priority) -> not_found.", 0),

            Clauses2 = generate_clauses_for_get_flows(TagList, Flows) ++ [DefaultClause2],
            AST3 = pt_lib:add_function(AST2, ast("get_flows [...$Clauses2...].", 0)),
            case compile:forms(AST3, [binary, return_errors]) of
                {ok, IfaceName, Binary} ->
                    ?INT_DBG("Compiled iface module for ~p: ~p", [ModuleName, IfaceName]),
                    {IfaceName, Binary};
                Error ->
                    ?INT_ERR("log interface module compilation error: ~p", [Error]),
                    erlang:error(log_iface_module_compilation_error, Error)
            end;
        _ -> undefined
    end.

get_appropriate_flows_exclusion(KeyModule, Rules) ->
    KeyStrModule = atom_to_list(KeyModule),
    lists:filter(fun(#rule{rule_str = RuleStr}) ->
                    ListBoolRule = rule_match3(RuleStr, KeyStrModule),
                    find_signal_flag(ListBoolRule)
                end, Rules).

get_appropriate_flows(_Key, _Priority, null) -> [];
get_appropriate_flows([], _Priority, _Rules) -> [];
get_appropriate_flows(Key, Priority, Rules) when is_atom(Key) ->
    ?INT_DBG("get_appropriate_flows for: ~p", [Key]),
    KeyStr = atom_to_list(Key),
    F = fun(#rule{rule_str = RuleStr, log_priority = LPriority, flows = Flows}, R) ->
            case LPriority >= Priority andalso rule_match(RuleStr, KeyStr) of
                true -> [Flows, R];
                _else -> R
            end
        end,
    lists:usort(lists:flatten(lists:foldl(F, [], Rules)));
get_appropriate_flows([K|_] = Keys, Priority, Rules) when is_list(Keys), is_atom(K) ->
    ?INT_DBG("get_appropriate_flows for: ~p", [Keys]),
    lists:usort(lists:flatten([ get_appropriate_flows(Key, Priority, Rules) ||
                                Key <- Keys ]));
get_appropriate_flows(Key, _, _) ->
    erlang:error({bad_tags, Key}).

%% @doc Rule is list of tree, key is string
-spec rule_match(Rule :: rule(), Key :: string()) -> boolean().
rule_match(Rule, Key) when is_list(Rule) ->
    rule_match2(Key, tokens(Rule));
rule_match({"!", {Rule}}, Key) ->
    not rule_match(Rule, Key);
rule_match({"&", {Op1, Op2}}, Key) ->
    rule_match(Op1, Key) andalso rule_match(Op2, Key);
rule_match({"|", {Op1, Op2}}, Key) ->
    rule_match(Op1, Key) orelse rule_match(Op2, Key).

%% TODO: !?
tokens([$! | Tail]) ->
    {Res, TmpList} = tokens_(Tail),
    ["!", TmpList | Res];
tokens(Rule) ->
    {Res, TmpList} = tokens_(Rule),
    [TmpList | Res].
tokens_(Rule) ->
    lists:foldr(
      fun (Char, {Res, TmpList}) when Char == $* orelse Char == $? ->
              {[ [Char], TmpList | Res ], []};
          (Char, {Res, TmpList}) ->
              {Res, [Char | TmpList]}
      end, {[], []}, Rule).

rule_match3(Rule, Key) when is_list(Rule) ->
    rule_match2(Key, tokens(Rule));
rule_match3({"!", {Rule}}, Key) ->
    case rule_match3(Rule, Key) of
        true ->
            {false, global};
        Res ->
            not Res
    end;
rule_match3({"&", {Op1, Op2}}, Key) ->
    [rule_match3(Op1, Key), rule_match3(Op2, Key)];
rule_match3({"|", {Op1, Op2}}, Key) ->
    [rule_match3(Op1, Key), rule_match3(Op2, Key)].

rule_match2(Key, ["!" | TokensTail]) ->
    not rule_match2(Key, TokensTail);
rule_match2([_ | KeyTail], ["?" | TokensTail]) ->
    rule_match2(KeyTail, TokensTail);
rule_match2([], ["?" | _]) ->
    false;
rule_match2(Key, ["*" , [] | TokensTail]) ->
    rule_match2(Key, ["*" | TokensTail]);
rule_match2(Key, ["*" , RuleToken | TokensTail] = Rule) ->
    case string:str(Key, RuleToken) of
        0 -> false;
        N ->
            KeyLeft = string:substr(Key, N + string:len(RuleToken)),
            case rule_match2(KeyLeft, TokensTail) of
                false -> rule_match2(KeyLeft, Rule);
                R -> R
            end
    end;
rule_match2(_, ["*"]) ->
    true;
rule_match2(Key, [[] | TokensTail]) ->
    rule_match2(Key, TokensTail);
rule_match2([KeyChar | KeyTail], [[RuleChar | RuleTokenTail] | TokensTail]) ->
    case KeyChar =:= RuleChar of
        true -> rule_match2(KeyTail, [RuleTokenTail | TokensTail]);
        false -> false
    end;
rule_match2([], []) ->
    true;
rule_match2(_, _) ->
    false.

generate_clauses(TagList, FlowsList) ->
    lists:foldl(
    fun (Tags, Acc) ->
        lists:foldl(
            fun (Priority, Acc2) ->
                {_, FlowsPriorities} = lists:keyfind(Tags, 1, FlowsList),
                {_, Flows} = lists:keyfind(Priority, 1, FlowsPriorities),
                case Flows of
                    [] ->
                        [ast("(@Priority = Priority, @Tags, Module, Line, File, Function, Format, Args, Self, Now) -> ok.", 0) | Acc2];
                    _ ->
                        Acc2
                end
            end, Acc, [?P_ERROR, ?P_WARNING, ?P_INFO, ?P_TRACE, ?P_DEBUG])
    end, [], TagList).

generate_clauses_for_get_flows(TagList, FlowsList) ->
    [
        begin
            ast("(@Tags, @Priority) -> @Flows.", 0)
        end
    ||
        Tags <- TagList,
        {Tags2, FlowsPriorities} <- FlowsList,
        {Priority, Flows} <- FlowsPriorities,
        Priority2 <- [?P_ERROR, ?P_WARNING, ?P_INFO, ?P_TRACE, ?P_DEBUG],
        Priority2 == Priority,
        Tags2 == Tags
    ].

find_signal_flag([{false,global} | _]) ->
    false;
find_signal_flag([Res | Tail]) ->
    case find_signal_flag(Res) of
        true ->
            find_signal_flag(Tail);
        _ ->
            false
    end;
find_signal_flag(_) ->
    true.


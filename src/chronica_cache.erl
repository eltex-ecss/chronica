-module(chronica_cache).

-include("chronica_int.hrl").
-include("chronica_config.hrl").
-include_lib("pt_scripts/include/pt_macro.hrl").
-include_lib("pt_lib/include/pt_lib.hrl").

-export(
   [
    load/2,
    save/3
   ]).

load(CacheDir, ConfigHash) ->
    Filename = cache_filename(CacheDir, ConfigHash),
    case file:read_file(Filename) of
        {ok, Binary} ->
            try
                Cache = erlang:binary_to_term(Binary),
                {BeamsCache, Cache1} =
                    case Cache of
                        [{beam_cache, Hash} | TailCache] ->
                            {{beam_cache, Hash}, TailCache};
                        _ ->
                            {{beam_cache, undef}, Cache}
                    end,
                case BeamsCache == cache_beams() of
                    true ->
                        Cache1;
                    false ->
                        []
                end
            catch
                _:_ ->
                    ?INT_ERR("Broken chronica cache in the file (~p)", [Filename]),
                    []
            end;
        {error, enoent} -> [];
        {error, Error} ->
            ?INT_ERR("Load chronica cache (~p) error: ~p", [Filename, Error]),
            []
    end.

save(_CacheDir, _ConfigHash, []) -> ok;
save(CacheDir, ConfigHash, Cache) ->
    Filename = cache_filename(CacheDir, ConfigHash),
    CacheBeams = cache_beams(),
    Binary = erlang:term_to_binary([CacheBeams | Cache]),
    Size = erlang:byte_size(Binary),
    ?INT_DBG("Save chronica cache (~b bytes) to ~p", [Size, Filename]),
    case file:write_file(Filename, Binary) of
        ok ->
            ok;
        {error, Error} ->
            ?INT_ERR("Save cache to ~10000000p error: ~p", [Filename, Error]),
            {error, Error}
    end.

cache_beams() ->
    Dir = filename:dirname(code:which(chronica)),
    Beams = filelib:wildcard(filename:join(Dir, "*.beam")),
    VSNs = lists:foldl(
        fun(Beam, Acc) ->
            try
                {ok, {_, [{attributes, Attr}]}} = beam_lib:chunks(Beam, [attributes]),
                case proplists:get_value(vsn, Attr) of
                    undefined -> Acc;
                    [VSN] -> [VSN | Acc];
                    _ -> Acc
                end
            catch
                _:_ ->
                    Acc
            end
        end, [], Beams),
    Hash = erlang:phash2(VSNs, 999999),
    {beam_cache, lists:flatten(io_lib:format("~6.10.0B", [Hash]))}.

cache_filename(CacheDir, ConfigHash) ->
    S = bin2hex(erlang:binary_to_list(ConfigHash)),
    filename:join(CacheDir, S).

bin2hex(Hex) when is_list(Hex) ->
     F = fun (16#0) -> $0;
             (16#1) -> $1;
             (16#2) -> $2;
             (16#3) -> $3;
             (16#4) -> $4;
             (16#5) -> $5;
             (16#6) -> $6;
             (16#7) -> $7;
             (16#8) -> $8;
             (16#9) -> $9;
             (16#A) -> $A;
             (16#B) -> $B;
             (16#C) -> $C;
             (16#D) -> $D;
             (16#E) -> $E;
             (16#F) -> $F
         end,

    F2 = fun(I, A) ->
        H = F(I bsr 4),
        L = F(I band 16#F),
        [H|[L|A]]
     end,

    lists:foldl(F2, [], lists:reverse(Hex)).

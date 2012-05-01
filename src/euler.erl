-module(euler).
-export([walk_subsets/3]).
-export([lazy_primes/1, load_primes/2, load_n_primes/2, load_pi_n/2]).
-export([primes_provider_new/1, primes_provider_delete/1, primes_provider_next/1]).

walk_subsets([], _, Acc) -> Acc;
walk_subsets([ X ], F, Acc) -> F([ X ], Acc);
walk_subsets([ _, _ | _ ] = List, F, Acc) ->
    S = list_to_tuple(List),
    Ne = tuple_size(S),
    Max = 1 bsl Ne,
    walk_subsets(S, Ne, Max, 0, F, Acc).
walk_subsets(_, _, Max, Max, _, Acc) -> Acc;
walk_subsets(S, Ne, Max, I, F, Acc) ->
    walk_subsets(S, Ne, Max, I + 1, F, F(walk_elements(S, Ne, I, 0, []), Acc)).
walk_elements(_, Size, _, I, Acc) when I >= Size -> Acc;
walk_elements(Set, Size, Mask, I, Acc) when (1 bsl I) band Mask =/= 0 ->
    walk_elements(Set, Size, Mask, I + 1, [ element(I + 1, Set) | Acc ]);
walk_elements(Set, Size, Mask, I, Acc) ->
    walk_elements(Set, Size, Mask, I + 1, Acc).

lazy_primes(FileName) ->
    { ok, File } = file:open(FileName, [read]),
    lazy_primes_loop(File).

lazy_primes_loop(File) ->
    fun() ->
        case io:get_line(File, "") of
            eof  -> file:close(File), [];
            Line ->
                case string:to_integer(Line) of
                    { P, _ } -> [ P | lazy_primes_loop(File) ];
                    _ -> file:close(File), []
                end
        end
    end.

load_primes(N, FileName) ->
    { ok, File } = file:open(FileName, [read]),
    lists:reverse(loop:while(
        fun (L) ->
            case io:get_line(File, "") of
                eof -> { false, L };
                Line ->
                    begin
                        case string:to_integer(Line) of
                            { I, _ } when I =< N -> { true, [I | L ] };
                            _ -> { false, L }
                        end
                    end
            end
        end,
        [])).

load_n_primes(N, FileName) -> 
    { ok, File } = file:open(FileName, [read]),
    lists:reverse(loop:while(
        fun ({ S, L }) ->
            if
                S >= N -> { false, L };
                true ->
                    case io:get_line(File, "") of
                        eof -> { false, L };
                        Line ->
                            { P, _ } = string:to_integer(Line),
                            { true, { S + 1, [P | L] } }
                    end
            end
        end,
        { 0, [] })).

load_pi_n(N, FileName) ->
    { ok, File } = file:open(FileName, [read]),
    loop:while(
        fun ({ S, A }) ->
            if
                S >= N -> { false, A };
                true ->
                    case io:get_line(File, "") of
                        eof -> { false, A };
                        Line ->
                            { V, _ } = string:to_integer(Line),
                            { true, { S + 1, array:set(S, V, A) } }
                    end
            end
        end,
        { 0, array:new() }).

%% Primes provider
primes_provider_new(FileName) ->
    { ok, File } = file:open(FileName, [read]),
    spawn(fun() -> primes_provider(File) end).

primes_provider_delete(Provider) -> Provider ! stop.

primes_provider_next(PrimesProvider) ->
    PrimesProvider ! self(),
    receive
        { Pid, P } when Pid == PrimesProvider -> P
    after 500 -> error({ "No response from primes provider" })
    end.

primes_provider(File) ->
    receive
        Parent when is_pid(Parent) ->
            case io:get_line(File, "") of
                eof -> Parent ! no_more, file:close(File);
                Line -> Parent ! { self(), element(1, string:to_integer(Line)) } , primes_provider(File)
            end;
        stop -> file:close(File)
    end.

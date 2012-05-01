-module(rationals).
-export([new/2, proper/1, is_proper/1, is_greater/2, is_lesser/2, max/2, min/2, neg/1, inv/1, add/2, sub/2, mul/2, rdiv/2, fast_add/2, fast_sub/2, fast_mul/2, fast_div/2]).
-export([heron/3]).

%%% Rational numbers are simply tuples { int(), int() }

new(N, D) -> G = intar:gcd(N, D), { N div G, D div G }.

proper({ N, D }) -> new(N, D).

is_greater({ N1, D1 }, { N2, D2 }) -> N1 * D2 - N2 * D1 > 0.

is_lesser({ N1, D1 }, { N2, D2 }) -> N1 * D2 - N2 * D1 < 0.

max(A, B) ->
    case is_greater(A, B) of
        true  -> A;
        false -> B
    end.

min(A, B) ->
    case is_greater(A, B) of
        true  -> B;
        false -> A
    end.

is_proper({ N, D }) -> intar:gcd(N, D) =:= 1.

neg({ N, D }) -> { -N, D }.

inv({ N, D }) -> { D, N }.

add(A, B) -> proper(fast_add(A, B)).

sub(A, B) -> proper(fast_sub(A, B)).

mul(A, B) -> proper(fast_mul(A, B)).

rdiv(A, B) -> proper(fast_div(A, B)).

fast_add({ N1, D1 }, { N2, D2 }) -> { N1 * D2 + N2 * D1, D1 * D2 }.

fast_sub(A, B) -> fast_add(A, neg(B)).

fast_mul({ N1, D1 }, { N2, D2 }) -> { N1 * N2, D1 * D2 }.

fast_div(A, B) -> fast_mul(A, inv(B)).

heron({ _, _ } = RNum, X0, N) -> heron(RNum, { X0, 1 }, 0, N);
heron(Num, X0, N) -> heron({ Num, 1 }, X0, N).

heron(_, X, N, N) -> X;
heron(R, Xk, K, N) -> heron(R, mul({ 1, 2 }, add(Xk, rdiv(R, Xk))), K + 1, N).

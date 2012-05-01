-module(loop).
-export([for/2, for/3, for/4, for/5, while/1, while/2]).

%% FOR functions

for(N, F) when is_function(F, 0) -> for(0, N, 1, ok, fun(_, _) -> F(), ok end);
for(N, F) when is_function(F, 1) -> for(0, N, 1, ok, fun(I, _) -> F(I), ok end).

for(N, A, F) when is_function(F, 2) -> for(0, N, 1, A, F);
for(I, N, F) when is_function(F, 1) -> for(I, N, 1, ok, fun(K, _) -> F(K), ok end).

for(I, N, A, F) when is_function(F, 2) -> for(I, N, 1, A, F).

for(I, End, _, AccIn, _) when I >= End -> AccIn;
for(I, End, Inc, AccIn, F) -> for(I + Inc, End, Inc, F(I, AccIn), F).


%% WHILE

while(F) when is_function(F, 0) -> while(fun(S) -> { F(), S } end, ok).

while(F, StateIn) when is_function(F, 1) ->
    case F(StateIn) of
        { true,  StateOut } -> while(F, StateOut);
        { false, StateOut } -> StateOut
    end.

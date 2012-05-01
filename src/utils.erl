-module(utils).
-export([chrono/1, plaunch/2, plaunch/3, shuffle/1]).

%% Parallel launch
plaunch(F, N) -> plaunch(F, N, fun(R) -> R end).
plaunch(F, N, Collect) -> Collect(plists:pmap(F, [ { I, N } || I <- lists:seq(1, N) ])).

%% Time a function
chrono(F) when is_function(F, 0) ->
    { T, V } = timer:tc(F),
    io:format("~n----~nReturned: ~w~nTime: ~ws~n", [ V, T / 1000000 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% shuffle(List1) -> List2
%% Takes a list and randomly shuffles it. Relies on random:uniform
%% Taken from trapexit.org
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shuffle(List) ->
%% Determine the log n portion then randomize the list.
   randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) -> randomize(List);
randomize(T, List) ->
    lists:foldl(fun(_E, Acc) -> randomize(Acc) end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
    D = lists:map(fun(A) -> {random:uniform(), A} end, List),
    {_, D1} = lists:unzip(lists:keysort(1, D)), 
    D1.

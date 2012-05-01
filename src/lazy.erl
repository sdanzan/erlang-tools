-module(lazy).
-export([all/2, any/2, expand/1]).

%% all
all(Pred, LazyList) ->
    case LazyList() of
        [] -> true;
        [ H | LazyTail ] ->
            case Pred(H) of
                true  -> all(Pred, LazyTail);
                false -> false
            end
    end.

%% any
any(Pred, LazyList) ->
    case LazyList() of
        [] -> false;
        [ H | LazyTail ] ->
            case Pred(H) of
                false -> any(Pred, LazyTail);
                true  -> true
            end
    end.

%% expand - use with care!
expand(LazyList) -> expand([], LazyList).
expand(List, LazyList) ->
    case LazyList() of
        [] -> lists:reverse(List); 
        [H | LazyTail] -> expand([H | List], LazyTail)
    end.

%% fold

%% map

%% filter

%% takewhile

%% dropwhile
%takewhile(Pred, LazyList) ->
%    case LazyList() of
%        [ H | LazyTail ]

-module(plists).
-export([pmap/2]).

%% Map function F over list L in parallel.
pmap(F, [ _ | _ ] = L) ->
    Parent = self(),
    [ receive { Pid, Result } -> Result end || Pid <- [ spawn_link(fun() -> Parent ! { self(), catch(F(Mapped)) } end) || Mapped <- L ] ].


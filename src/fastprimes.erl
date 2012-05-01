-module(fastprimes).
-export([primes/1, countprimes/1]).
-on_load(init/0).

init() -> ok = erlang:load_nif("fastprimes", 0).

primes(_) -> exit(fastprimes_nif_library_not_loaded).
countprimes(_) -> exit(fastprimes_nif_library_not_loaded).

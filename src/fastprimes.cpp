#include <vector>

#include "erl_nif.h"

typedef ErlNifUInt64 ulong_t;
typedef std::vector<bool> vsieve_t;

namespace {

// Integer square root: nearest integer
// smaller or equal than square root of N 
ulong_t
int_sqrt(ulong_t n)
{
    ulong_t sqrmin = 1;
    ulong_t sqrmax = n;
    while (sqrmin != sqrmax - 1)
    {
        ulong_t sqr = (sqrmax + sqrmin) >> 1;
        if (sqr * sqr > n)
        {
            sqrmax = sqr;
        }
        else
        {
            sqrmin = sqr;
        }
    }
    return sqrmin;
}

// Simple sieve of Eratosthenes
// Returns a bool vector with bits
// set to 1 for each prime up to N.
vsieve_t
primes(ulong_t n)
{
    vsieve_t sieve(n + 1, true);
    sieve[0] = sieve[1] = false;
    for(std::size_t i = 4; i < sieve.size(); i += 2)
        sieve[i] = false;

    ulong_t lim = int_sqrt(n);
    for (std::size_t i = 3; i <= lim; i += 2)
    {
        if (sieve[i])
        {
            for (std::size_t j = i * i; j <= n; j += i)
            {
                sieve[j] = false;
            }
        }
    }

    return sieve;
}

}

extern "C"
{

// Returns a erlang list of primes up to N
// N sits in argv[0]
static
ERL_NIF_TERM
nif_primes (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ulong_t n = 0;
    if (!enif_get_uint64 (env, argv[0], &n))
        return enif_make_badarg(env);

    vsieve_t r = primes(n);
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (vsieve_t::const_reverse_iterator iter = r.rbegin();
         iter != r.rend();
         ++iter, --n)
    {
        if (*iter)
        {
            list = enif_make_list_cell(env, enif_make_uint64(env, n), list);
        }
    }

    return list;
}

// Count primes, just for bench
static
ERL_NIF_TERM
nif_countprimes (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ulong_t n = 0;
    if (!enif_get_uint64 (env, argv[0], &n))
        return enif_make_badarg(env);

    vsieve_t r = primes(n);
    ulong_t c = 1;
    for (vsieve_t::const_iterator iter = r.begin() + 3;
         iter != r.end();
         iter += 2)
    {
        if (*iter)
        {
            ++c;
        }
    }

    return enif_make_uint64(env, c);
}

static
ErlNifFunc nif_funcs[] =
{
    { "primes",      1, &nif_primes },
    { "countprimes", 1, &nif_countprimes },
};

static
int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static
int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static
int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static
void
unload(ErlNifEnv* env, void* priv)
{
    return;
}

ERL_NIF_INIT(fastprimes, nif_funcs, &load, &reload, &upgrade, &unload);

}

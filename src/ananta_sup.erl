%%%-------------------------------------------------------------------
%%% @doc Supervisor for Ananta KV Store
%%%-------------------------------------------------------------------
-module(ananta_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    KVChild = {
        ananta_kv_server,
        {ananta_kv_server, start_link, []},
        permanent,
        5000,
        worker,
        [ananta_kv_server]
    },
    {ok, { {one_for_one, 5, 10}, [KVChild] } }.


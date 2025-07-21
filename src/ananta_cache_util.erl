%% ananta_cache_util.erl
%% Utility module for ETS-based caching
-module(ananta_cache_util).

-export([init/0, set_cluster_members/1, get_cluster_members/0]).

-define(CLUSTER_TABLE, cluster).

init() ->
    case ets:info(?CLUSTER_TABLE) of
        undefined ->
            ets:new(?CLUSTER_TABLE, [named_table, set, public]);
        _ ->
            ok
    end.

set_cluster_members(Members) ->
    ets:insert(?CLUSTER_TABLE, {members, Members}).

get_cluster_members() ->
    case ets:lookup(?CLUSTER_TABLE, members) of
        [{members, Members}] -> Members;
        _ -> []
    end.


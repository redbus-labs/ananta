%% ananta_cluster_server.erl
%% Simple cluster server with ndm:ping functionality
-module(ananta_cluster_server).
-behaviour(gen_server).

%% API
-export([start_link/0, join/1, leave/1, members/0, ndm_ping/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {members = []}).

%%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

join(Node) ->
    gen_server:call(?MODULE, {join, Node}).

leave(Node) ->
    gen_server:call(?MODULE, {leave, Node}).

members() ->
    gen_server:call(?MODULE, members).

%%% gen_server callbacks
init([]) ->
    ananta_cache_util:init(),
    %% Start periodic ping timer (every 60_000 ms = 1 min)
    erlang:send_after(60_000, self(), check_nodes),
    {ok, #state{}}.

handle_call({join, Node}, _From, State) ->
    net_kernel:connect_node(Node),
    Members = lists:usort([Node | State#state.members]),
    ananta_cache_util:set_cluster_members(Members),
    %% Chunked replication of KV data to new node
    ExistingNodes = lists:delete(Node, Members),
    case ExistingNodes of
        [SourceNode | _] ->
            replicate_kv_chunks(SourceNode, Node);
        [] ->
            ok
    end,
    {reply, ok, State#state{members = Members}};
handle_call({leave, Node}, _From, State) ->
    Members = lists:delete(Node, State#state.members),
    ananta_cache_util:set_cluster_members(Members),
    {reply, ok, State#state{members = Members}};
handle_call(members, _From, State) ->
    Members = ananta_cache_util:get_cluster_members(),
    {reply, Members, State};
handle_call(_Req, _From, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_nodes, State) ->
    Members = State#state.members,
    AliveNodes = check_and_update_nodes(Members),
    ananta_cache_util:set_cluster_members(AliveNodes),
    %% Restart timer
    erlang:send_after(60_000, self(), check_nodes),
    {noreply, State#state{members = AliveNodes}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ndm:ping/1 implementation
%% Usage: ananta_cluster_server:ndm_ping(Node).
ndm_ping(Node) ->
    net_adm:ping(Node).

%%% Helper function to check nodes
check_and_update_nodes(Nodes) ->
    lists:foldl(fun(Node, Acc) ->
        lager:info("Checking node: ~p", [Node]),
        case net_adm:ping(Node) of
            pong ->
                lager:info("pong node: ~p", [Node]),
                %% Ensure connection
                net_kernel:connect_node(Node),
                [Node | Acc];
            pang ->
                lager:info("pang node: ~p", [Node]),
                %% Node is unreachable, do not add
                Acc
        end
    end, [], Nodes).

%% Helper function for chunked KV replication
replicate_kv_chunks(SourceNode, TargetNode) ->
    replicate_kv_chunks(SourceNode, TargetNode, undefined).

replicate_kv_chunks(SourceNode, TargetNode, Continuation) ->
    case rpc:call(SourceNode, ananta_kv_server, fetch_chunk, [Continuation]) of
        {KVPairs, '$end_of_table'} ->
            rpc:call(TargetNode, ananta_kv_server, replicate, [KVPairs]);
        {KVPairs, NextCont} ->
            rpc:call(TargetNode, ananta_kv_server, replicate, [KVPairs]),
            replicate_kv_chunks(SourceNode, TargetNode, NextCont)
    end.

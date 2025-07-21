%%%-------------------------------------------------------------------
%%% @doc Simple KV Store gen_server for Ananta
%%%-------------------------------------------------------------------
-module(ananta_kv_server).
-behaviour(gen_server).

%% API
-export([start_link/0, get/1, put/2, delete/1, fetch_all/0, replicate/1, fetch_chunk/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

    -record(state, {ets_table, lru_table, lru_counter = 0}).

%%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

put(Key, Value) ->
    gen_server:call(?MODULE, {put, Key, Value}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

%% Fetch all KV pairs from ETS table
fetch_all() ->
    ets:tab2list(?MODULE).

%% Replicate KV pairs to local ETS table
replicate(KVPairs) ->
    gen_server:call(?MODULE, {replicate, KVPairs}).

%% Fetch a chunk of KV pairs from ETS table using select/3
fetch_chunk(Continuation) ->
    Pattern = {'$1', '$2'},
    Limit = 1000, %% Tune this chunk size as needed
    case Continuation of
        undefined ->
            ets:select(?MODULE, [{Pattern, [], [{{'$1', '$2'}}]}], Limit);
        _ ->
            ets:select(Continuation)
    end.

%%% gen_server callbacks
init([]) ->
    Table = ets:new(?MODULE, [named_table, set, public]),
    LruTable = ets:new(ananta_kv_lru, [named_table, ordered_set, public]),
    {ok, #state{ets_table = Table, lru_table = LruTable}}.

handle_call({get, Key}, _From, State) ->
    NewCounter = State#state.lru_counter + 1,
    ets:insert(State#state.lru_table, {Key, NewCounter}),
    State1 = State#state{lru_counter = NewCounter},
    case ets:lookup(State#state.ets_table, Key) of
        [{_, V}] -> {reply, V, State1};
        _ -> {reply, undefined, State1}
    end;
handle_call({put, Key, Value}, _From, State) ->
    Members = ananta_cluster_server:members(),
    Quorum = max(1, (length(Members) div 2) + 1),
    Results = [rpc:call(Node, ets, insert, [?MODULE, {Key, Value}]) || Node <- Members],
    LocalResult = ets:insert(State#state.ets_table, {Key, Value}),
    %% LRU update
    NewCounter = State#state.lru_counter + 1,
    ets:insert(State#state.lru_table, {Key, NewCounter}),
    State1 = State#state{lru_counter = NewCounter},
    %% LRU truncation if needed
    State2 = maybe_truncate_lru(State1),
    case count_success([LocalResult | Results]) >= Quorum of
        true -> {reply, ok, State2};
        false -> {reply, {error, quorum_failed}, State2}
    end;
handle_call({delete, Key}, _From, State) ->
    Members = ananta_cluster_server:members(),
    Quorum = max(1, (length(Members) div 2) + 1),
    Results = [rpc:call(Node, ets, delete, [?MODULE, Key]) || Node <- Members],
    LocalResult = ets:delete(State#state.ets_table, Key),
    ets:delete(State#state.lru_table, Key),
    case count_success([LocalResult | Results]) >= Quorum of
        true -> {reply, ok, State};
        false -> {reply, {error, quorum_failed}, State}
    end;
handle_call({replicate, KVPairs}, _From, State) ->
    lists:foreach(fun({K, V}) -> ets:insert(State#state.ets_table, {K, V}) end, KVPairs),
    {reply, ok, State};
handle_call(_Req, _From, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

count_success(Results) ->
    lists:foldl(fun
        (true, Acc) -> Acc + 1;
        ("true", Acc) -> Acc + 1;
        (<<"true">>, Acc) -> Acc + 1;
        (_, Acc) -> Acc
    end, 0, Results).

maybe_truncate_lru(State) ->
    Table = State#state.ets_table,
    LruTable = State#state.lru_table,
    MemoryWords = ets:info(Table, memory),
    WordSize = erlang:system_info(wordsize),
    SizeBytes = MemoryWords * WordSize,
    MaxBytes = 1024 * 1024 * 1024, %% 1GB
    case SizeBytes > MaxBytes of
        true ->
            %% Remove 10% of oldest keys at a time
            ToRemove = trunc(ets:info(Table, size) * 0.1),
            Oldest = ets:select(LruTable, [{'$1', [], ['$1']}], ToRemove),
            lists:foreach(fun(Key) ->
                ets:delete(Table, Key),
                ets:delete(LruTable, Key)
            end, Oldest),
            State;
        false ->
            State
    end.

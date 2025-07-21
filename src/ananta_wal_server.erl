%% ananta_wal_server.erl
%% Write-Ahead Log and Snapshotting gen_server
-module(ananta_wal_server).
-behaviour(gen_server).

%% API
-export([start_link/1, put/2, delete/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(WAL_FILE, "ananta_wal.log").
-define(SNAPSHOT_FILE, "ananta_snapshot.dat").
-define(SNAPSHOT_INTERVAL, 30 * 60 * 1000). %% 30 minutes in ms
-define(WAL_KEEP_SECS, 30 * 60). %% 30 minutes in seconds

-record(state, {
    wal = [],
    wal_file = ?WAL_FILE,
    snapshot_file = ?SNAPSHOT_FILE,
    timer_ref = undefined,
    ets_tables = []
}).

%%% API
start_link(Tables) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Tables], []).

put(Key, Value) ->
    gen_server:cast(?MODULE, {put, Key, Value}).

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

stop() ->
    gen_server:cast(?MODULE, stop).

%%% gen_server callbacks
init([Table]) ->
    load_snapshot(Table),
    WAL = load_wal(?WAL_FILE),
    replay_wal(WAL, Table),
    TimerRef = erlang:send_after(?SNAPSHOT_INTERVAL, self(), snapshot),
    {ok, #state{wal=WAL, timer_ref=TimerRef, ets_tables = [Table]}}.

handle_cast({put, Key, Value}, State) ->
    Now = erlang:system_time(second),
    Entry = {Now, put, Key, Value},
    ok = append_wal(State#state.wal_file, Entry),
    WAL1 = prune_wal([Entry | State#state.wal]),
    {noreply, State#state{ wal=WAL1}};

handle_cast({delete, Key}, State) ->
    Now = erlang:system_time(second),
    Entry = {Now, delete, Key},
    ok = append_wal(State#state.wal_file, Entry),
    WAL1 = prune_wal([Entry | State#state.wal]),
    {noreply, State#state{wal=WAL1}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Req, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {reply, error, State}.

handle_info(snapshot, State) ->
    lists:foreach(fun(Table) -> save_snapshot(Table) end, State#state.ets_tables),
    ok = truncate_wal(State#state.wal_file),
    TimerRef = erlang:send_after(?SNAPSHOT_INTERVAL, self(), snapshot),
    {noreply, State#state{wal=[], timer_ref=TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal helpers

append_wal(File, Entry) ->
    Bin = term_to_binary(Entry),
    file:write_file(File, <<Bin/binary, "\n">>, [append]).

prune_wal(WAL) ->
    Now = erlang:system_time(second),
    lists:filter(fun
                     ({TS,_,_,_}) -> TS > Now - ?WAL_KEEP_SECS;
                     ({TS,_,_}) -> TS > Now - ?WAL_KEEP_SECS
                 end, WAL).

save_snapshot(Table) ->
    TableName = ets:info(Table, name),
    File = atom_to_list(TableName) ++ ".tab",
    ets:tab2file(Table, File).

load_snapshot(Table) ->
    case file:read_file(?SNAPSHOT_FILE) of
        {ok, Bin} ->
            try
                {ok, Data} = binary_to_term(Bin),
                ets:delete_all_objects(Table),
                lists:foreach(fun({K, V}) -> ets:insert(Table, {K, V}) end, Data)
            catch _:_ ->
                ok
            end;
        _ -> ok
    end.

load_wal(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Lines = binary:split(Bin, <<"\n">>, [global]),
            [binary_to_term(L) || L <- Lines, L =/= <<>>];
        _ -> []
    end.

replay_wal(WAL, Table) ->
    lists:foreach(fun
        ({_, put, K, V}) -> ets:insert(Table, {K, V});
        ({_, delete, K}) -> ets:delete(Table, K);
        (_) -> ok
    end, WAL).

truncate_wal(File) ->
    file:write_file(File, <<>>).

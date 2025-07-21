%%%-------------------------------------------------------------------
%%% @doc OTP Application module for Ananta KV Store
%%%-------------------------------------------------------------------
-module(ananta_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @doc Start the application and supervisor
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(routes()),
    {ok, _} =  cowboy:start_clear(http, #{
        socket_opts => [{port, 8002}, {backlog, 1024}], num_acceptors => 32,
        max_connections => 10000},
        #{env => #{dispatch => Dispatch},
            %% TODO: stream_handlers => [stream_http_rest_log_handler],
            %%onresponse => fun log_utils:req_log/4,
            idle_timeout => 900000,
            %%linger_timeout => 180000,
            max_keepalive => 1000}),
    ananta_cache_util:init(),
    ananta_cluster_server:start_link(),
    ananta_sup:start_link().
    %% Start the KV server


routes() ->
    [{'_',
        [
            {"/kv/:key", ananta_kv_handler, []},
            {"/cluster/[...]", ananta_cluster_handler, []},
            {"/cluster", ananta_cluster_handler, []}
        ]
    }].

%%--------------------------------------------------------------------
%% @doc Stop the application
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

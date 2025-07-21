%% ananta_cluster_handler.erl
%% HTTP handler for cluster join, leave, and members operations
-module(ananta_cluster_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            handle_post(Req, State);
        <<"GET">> ->
            handle_get(Req, State);
        _ ->
            lager:info("Unsupported method: ~p", [cowboy_req:method(Req)]),
            {ok, cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req), State}
    end.

handle_post(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case cowboy_req:path(Req1) of
        <<"/cluster/join">> ->
            Node = binary_to_atom(Body, utf8),
            ok = ananta_cluster_server:join(Node),
            {ok, cowboy_req:reply(200, #{}, <<"joined">>, Req1), State};
        <<"/cluster/leave">> ->
            Node = binary_to_atom(Body, utf8),
            ok = ananta_cluster_server:leave(Node),
            {ok, cowboy_req:reply(200, #{}, <<"left">>, Req1), State};
        _ ->
            {ok, cowboy_req:reply(404, #{}, <<"Not Found">>, Req1), State}
    end.

handle_get(Req, State) ->
    case cowboy_req:path(Req) of
        <<"/cluster/members">> ->
            Members = ananta_cluster_server:members(),
            Json = jsx:encode(Members),
            {ok, cowboy_req:reply(200, #{"content-type" => "application/json"}, Json, Req), State};
        _ ->
            {ok, cowboy_req:reply(404, #{}, <<"Not Found">>, Req), State}
    end.


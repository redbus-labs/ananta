%% File: handlers/ananta_kv_handler.erl
-module(ananta_kv_handler).
-behaviour(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2, content_types_accepted/2, content_types_provided/2]).
-export([resource_exists/2, delete_resource/2, is_authorized/2]).
-export([get_kv/2, put_kv/2]).

init(Req, _Opts) ->
    {cowboy_rest, Req, #{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

is_authorized(Req, State) ->
    {true, Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/octet-stream">>, get_kv},
        {<<"application/json">>, get_kv}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, put_kv},
        {<<"text/plain">>, put_kv}], Req, State}.

resource_exists(Req, State) ->
    Key = cowboy_req:binding(key, Req),
    case ananta_kv_server:get(Key) of
        undefined -> {false, Req, State};
        _ -> {true, Req, State}
    end.

get_kv(Req, State) ->
    Key = cowboy_req:binding(key, Req),
    case ananta_kv_server:get(Key) of
        undefined -> {<<"not_found">>, Req, State};
        Value ->
            case erlang:is_binary(Value) of
                true -> {binary_to_term(Value), Req, State};
                false -> {Value, Req, State}
            end
    end.

put_kv(Req, State) ->
    Key = cowboy_req:binding(key, Req),
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    #{<<"value">> := Value} = jiffy:decode(Body, [return_maps]),
    ok = ananta_kv_server:put(Key, term_to_binary(Value)),
    {true, Req1, State}.

delete_resource(Req, State) ->
    Key = cowboy_req:binding(key, Req),
    ok = ananta_kv_server:delete(Key),
    {true, Req, State}.
%% File: tcp_kv_client.erl
-module(tcp_kv_client).
-export([get/3, put/4, delete/3]).

connect(Host, Port) ->
    gen_tcp:connect(Host, Port, [binary, {packet, line}, {active, false}]).

get(Host, Port, Key) ->
    {ok, Socket} = connect(Host, Port),
    gen_tcp:send(Socket, <<"GET ", Key/binary, "\n">>),
    {ok, Response} = gen_tcp:recv(Socket, 0),
    gen_tcp:close(Socket),
    Response.

put(Host, Port, Key, Value) ->
    {ok, Socket} = connect(Host, Port),
    %% Ensure Key and Value are binaries
    gen_tcp:send(Socket, <<"PUT ", Key/binary, " ", Value/binary, "\n">>),
    {ok, Response} = gen_tcp:recv(Socket, 0),
    gen_tcp:close(Socket),
    Response.

delete(Host, Port, Key) ->
    {ok, Socket} = connect(Host, Port),
    gen_tcp:send(Socket, <<"DELETE ", Key/binary, "\n">>),
    {ok, Response} = gen_tcp:recv(Socket, 0),
    gen_tcp:close(Socket),
    Response.
%% File: src/ananta_tcp_server.erl
-module(ananta_tcp_server).
-export([start/0, accept/1, handle_client/1]).

start() ->
    {ok, Listen} = gen_tcp:listen(8001, [binary, {packet, line}, {active, false}]),
    accept(Listen).

accept(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> handle_client(Socket) end),
    accept(Listen).

handle_client(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      Response = handle_command(Data),
      gen_tcp:send(Socket, Response),
      handle_client(Socket); % Continue to handle more commands
    {error, closed} ->
      gen_tcp:close(Socket);
    {error, _Reason} ->
      gen_tcp:close(Socket)
  end.

handle_command(<<"GET ", Rest/binary>>) ->
    Key = binary:part(Rest, {0, byte_size(Rest) - 1}),
    case ananta_kv_server:get(Key) of
        undefined ->
          EpochTime = integer_to_binary(erlang:system_time(microseconds)),
          <<"NOT_FOUND ", EpochTime/binary, "\n">>;
        Value ->
          <<"OK ", Value/binary, "\n">>
    end;
handle_command(<<"PUT ", KeyValue/binary>>) ->
  case binary:split(KeyValue, <<" ">>, [global]) of
    [Key, Value] ->
      Value1 = binary:part(Value, {0, byte_size(Value) - 1}),
      ananta_kv_server:put(Key, Value1),
      <<"OK\n">>;
    _ -> <<"ERROR\n">>
  end;
handle_command(<<"DELETE ", Key/binary>>) ->
    ananta_kv_server:delete(Key),
    <<"OK\n">>;
handle_command(_) ->
    <<"ERROR\n">>.
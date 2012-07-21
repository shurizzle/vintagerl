-module(vintagerl).
-compile(export_all).
-define(TCP_OPTION, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start() ->
  start(1337).

start(Port) ->
  io:format("Starting server on port ~p~n", [Port]),
  client_manager:start(),
  spawn(fun() ->
      {ok, Socket} = gen_tcp:listen(Port, ?TCP_OPTION),
      accept(Socket)
  end).

handle_client(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      client_manager:post_message(Socket, Data),
      vintagerl:handle_client(Socket);
    {error, closed} ->
      client_manager:disconnect(Socket)
  end.

accept(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  spawn(fun() -> vintagerl:handle_client(Socket) end),
  client_manager:connect(node(), Socket),
  accept(LSocket).

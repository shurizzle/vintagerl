-module(client_manager).
-define(SERVER, client_manager).
-compile(export_all).
-record(client, {name=none, socket, node, mode}).

start() ->
  global:trans({?SERVER, ?SERVER},
    fun() ->
        case global:whereis_name(?SERVER) of
          undefined ->
            Pid = spawn_link(node(), client_manager, manage_clients, [[]]),
            global:register_name(?SERVER, Pid);
          _ ->
            ok
        end
    end).

connect(Node, Socket) ->
  global:send(?SERVER, {connected, Node, Socket}).

disconnect(Socket) ->
  global:send(?SERVER, {disconnected, Socket}).

post_message(Socket, Data) ->
  global:send(?SERVER, {data, Socket, Data}).

find_client_by_socket(Socket, Clients) ->
  {value, Client} = lists:keysearch(Socket, #client.socket, Clients),
  Client.

name_exists(Name, Clients) ->
  case lists:keysearch(Name, #client.name, Clients) of
    {value, _} ->
      true;
    false ->
      false
  end.

broadcast(Clients, Format, Args) ->
  Data = io_lib:format(Format, Args),
  ActiveClients = lists:filter(fun(C) -> C#client.mode == active end, Clients),
  lists:foreach(fun(C) ->
        rpc:call(C#client.node, gen_tcp, send, [C#client.socket, Data])
    end, ActiveClients),
  ok.

without(Client, Clients) ->
  lists:keydelete(Client#client.socket, #client.socket, Clients).

prompt(Client) ->
  case Client#client.mode of
    active ->
      ok;
    connected ->
      rpc:call(Client#client.node, gen_tcp, send, [Client#client.socket, "Name: "])
  end.

strlen(Bin) when is_binary(Bin) ->
  strlen(Bin, 0).

strlen(<<>>, N) ->
  {N, <<>>};
strlen(<<0/integer, Rest/binary>>, N) ->
  {N, Rest};
strlen(<<_/integer, Rest/binary>>, N) ->
  strlen(Rest, N + 1).

to_string(Bin) when is_binary(Bin) ->
  {Size, _} = strlen(Bin),
  <<String:Size/binary, _/binary>> = Bin,
  binary_to_list(String);
to_string(X) -> X.

re_sub(S1, Pattern, S2) when is_list(Pattern) ->
  re_sub(S1, list_to_binary(Pattern), S2);
re_sub(Subj, Pattern, Subst) when is_binary(Pattern) ->
  case re:replace(Subj, Pattern, Subst) of
    [Res, []] ->
      binary_to_list(Res);
    _ ->
      Subj
  end.

trim(Str) ->
  re_sub(re_sub(Str, "^[ \t\r\n]*", ""), "[ \t\r\n]*$", "").

clean(Bin) ->
  trim(to_string(Bin)).

parse_data(Client, Clients, Data) ->
  case Client#client.mode of
    active ->
      broadcast(without(Client, Clients), "~s>> ~s\r~n", [Client#client.name, Data]),
      Clients;
    connected ->
      Name = clean(Data),
      case name_exists(Name, Clients) of
        true ->
          gen_tcp:send(Client#client.socket, io_lib:format("--- Name ~s already exists ---~n", [Name])),
          Clients;
        false ->
          NewClient = Client#client{name=Name, mode=active},
          gen_tcp:send(Client#client.socket, "--- You are now chatting ---\n"),
          broadcast(without(Client, Clients), "~s connected.\r~n", [Name]),
          [NewClient | without(Client, Clients)]
      end
  end.

manage_clients(Clients) ->
  receive
    {connected, Node, Socket} ->
      Client = #client{socket=Socket, node=Node, mode=connected},
      prompt(Client),
      NewClients = [Client | Clients];
    {disconnected, Socket} ->
      Client = find_client_by_socket(Socket, Clients),
      case Client#client.mode of
        active ->
          broadcast(without(Client, Clients), "~s left\r~n", [Client#client.name]);
        _ -> ok
      end,
      NewClients = lists:delete(Client, Clients);
    {data, Socket, Data} ->
      Client = find_client_by_socket(Socket, Clients),
      NewClients = parse_data(Client, Clients, re_sub(to_string(Data), "\r?\n$", "")),
      NewClient = find_client_by_socket(Socket, NewClients),
      prompt(NewClient)
  end,
  manage_clients(NewClients).

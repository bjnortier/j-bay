-module(ot_client_proxy).
-compile(export_all).

-record(state, {serverversion=0, resultingops=[]}).

start() ->
    State = #state{},
    spawn(fun() -> create_listener(State) end).

proxy(Browser, State0) ->
    receive
	{browser, Browser, Msg} ->
	    io:format("received: ~p~n", [Msg]),
	    ServerVersion = State0#state.serverversion,
	    Decoded = mochijson2:decode(Msg),
	    case Decoded of
		{struct, [{<<"clientVersion">>, ClientClientVersion},
			  {<<"serverVersion">>, ClientServerVersion},
			  {<<"ops">>, Ops}]} ->
		    
		    DocOps = to_docops(Ops),
		    
		    case ServerVersion of 
			ClientServerVersion ->
			    State1 = State0#state{
				       resultingops=State0#state.resultingops ++ DocOps,
				       serverversion=State0#state.serverversion + 1
				      },
			    io:format("New state: ~p~n", [State1]),
			    Browser ! {send, mochijson2:encode(
					       {struct, [{<<"ack">>, 
							  {struct, [
								    {<<"clientVersion">>, ClientClientVersion },
								    {<<"serverVersion">>, State1#state.serverversion},
								    {<<"applied_ops">>, length(Ops)}
								   ]}}]})},
			    proxy(Browser, State1);
			_ ->
			    io:format("?? : ~p~n", [State0]),
			    proxy(Browser, State0)
		    end;
		_ ->
		    
%% 		    Browser ! {send, mochijson2:encode(
%% 			       {struct, [{<<"ack">>, {struct, [
%% 							       {<<"ack_version">>, 0 },
%% 							       {<<"new_server_version">>, 0}
%% 							      ]}}]})},
		    %%timer:sleep(1000),
		    proxy(Browser, State0)
	    end;
	_ -> proxy(Browser, State0)
    end.




create_listener(State) ->
    {ok, Listen} = gen_tcp:listen(1234, [{packet,0},
					 {reuseaddr,true},
					 {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    wait(Socket, State).

    
wait(Socket, State) ->
    receive
	{tcp, Socket, Data} ->
	    io:format("Handshake: ~p~n", [Data]),
	    Handshake =
		[
		 "HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
		 "Upgrade: WebSocket\r\n",
		 "Connection: Upgrade\r\n",
		 "WebSocket-Origin: file://\r\n",
		 "WebSocket-Location: ",
		 "  ws://localhost:1234/websession\r\n\r\n"
		],
	    gen_tcp:send(Socket, Handshake),
	    Browser = self(),
	    Pid = spawn_link(fun() -> proxy(Browser, State) end),
	    loop(zero, Socket, Pid);
	Any ->
	    io:format("??? Received:~p~n",[Any]),
	    wait(Socket, State)
    end.

loop(Buff, Socket, Pid) ->
    receive
	{tcp, Socket, Data} ->
	    handle_data(Buff, Data, Socket, Pid);
	{tcp_closed, Socket} ->
	    io:format("Connection closed from client~n"),
	    Pid ! {browser_closed, self()};
	{send, Data} ->
	    gen_tcp:send(Socket, [0,Data,255]),
	    loop(Buff, Socket, Pid);
	Any ->
	    io:format("Received:~p~n",[Any]),
	    loop(Buff, Socket, Pid)
    end.

handle_data(zero, [0|T], Socket, Pid) ->
    handle_data([], T, Socket, Pid);
handle_data(zero, [], Socket, Pid) ->
    loop(zero, Socket, Pid);
handle_data(Message, [255|T], Socket, Pid) ->
    handle_message(lists:reverse(Message), Pid),
    handle_data(zero,T, Socket, Pid);
handle_data(L, [H|T], Socket, Pid) ->
    handle_data([H|L], T, Socket, Pid);
handle_data([], L, Socket, Pid) ->
    loop(L, Socket, Pid).

handle_message(Message, ProxyPid) ->
    ProxyPid ! {browser, self(), Message}.


to_docops(List) when is_list(List) ->
    lists:map(fun to_docops/1, List);
to_docops({struct, [{K, V}]}) -> 
    {to_key(K), to_val(V)}.

to_key(<<"ret">>) -> 
    ret;
to_key(<<"ins">>) -> 
    ins;
to_key(<<"del">>) -> 
    del.

to_val(V) when is_binary(V) ->
    binary_to_list(V);
to_val(V) ->
    V.

	    

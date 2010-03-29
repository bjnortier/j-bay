-module(client_node).
-compile(export_all).

%% start()
%%  This should be in another module for clarity
%%  but is included here to make the example self-contained

start() ->
    F = fun interact/2,
    spawn(fun() -> start(F, 0) end).

interact(Browser, State) ->
    receive
	{browser, Browser, Str} ->
	    io:format("~p~n", [Str]),
	    Browser ! {send, mochijson2:encode(<<"ack">>)},
	    interact(Browser, State)
    end.


start(F, State0) ->
    {ok, Listen} = gen_tcp:listen(1234, [{packet,0},
					 {reuseaddr,true},
					 {active, true}]),
    par_connect(Listen, F, State0).

par_connect(Listen, F, State0) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen, F, State0) end),
    wait(Socket, F, State0).

wait(Socket, F, State0) ->
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
	    Pid = spawn_link(fun() -> F(Browser, State0) end),
	    loop(zero, Socket, Pid);
	Any ->
	    io:format("Received:~p~n",[Any]),
	    wait(Socket, F, State0)
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

handle_message(Message, InteractPid) ->
    DocOps = json_to_docops(Message),
    InteractPid ! {browser, self(), DocOps}.


json_to_docops(JSON) ->
    Decoded = mochijson2:decode(JSON),    
    to_docops(Decoded).

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

	    

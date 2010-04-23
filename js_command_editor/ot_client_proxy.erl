-module(ot_client_proxy).
-compile(export_all).

-record(state, {serverversion=0, resultingops=[]}).

start() ->
    State = #state{},
    spawn(fun() -> create_listener(State) end).


component_to_json_format({Type, String}) when is_list(String) ->
    {Type, list_to_binary(String)};
component_to_json_format(X) ->
    X.
    

ops_to_json_format(Ops) ->
    lists:map(fun(Op) ->
 		      lists:map(fun(Component) ->
 					{struct, [component_to_json_format(Component)]}
 				end,
 				Op)
	      end,
	      Ops).

replace_x_str([Hd|Rest], Replaced) ->
    case Hd of
	$X ->
	    replace_x_str(Rest, Replaced ++ [{del, "X"},{ins, "Y"}]);
	_ -> 
	    replace_x_str(Rest, Replaced ++ [{ret, 1}])
    end;
replace_x_str([], Replaced) ->
    Replaced.

replace_x(Browser, State0) ->
    timer:sleep(500),
    DocOps = State0#state.resultingops,
    Document = composer:apply_empty(DocOps),
    
    case lists:member($X, Document) of
	true ->
	    NewOps = [ot:compress(replace_x_str(Document, []))],
	    State1 = State0#state{
		       resultingops=State0#state.resultingops ++ NewOps,
		       serverversion=State0#state.serverversion + 1
		      },
	    Browser ! {send, mochijson2:encode(
			       {struct, [{<<"ops">>, 
					  {struct, [
						    {<<"serverVersion">>,  
						     State1#state.serverversion},
						    {<<"applied_ops">>,
						     ops_to_json_format(NewOps)}
						   ]}}]})},
	    proxy(Browser, State1);
	false ->
	    proxy(Browser, State0)
    end.
    

proxy(Browser, State0) ->
    io:format("Document: ~p~n", [ composer:apply_empty(State0#state.resultingops)]),


    receive
	{browser, Browser, Msg} ->
	    io:format("received: ~p~n", [Msg]),
	    ServerVersion = State0#state.serverversion,
 	    ServerOps = State0#state.resultingops,
	    Decoded = mochijson2:decode(Msg),
	    case Decoded of
		{struct,[{<<"transform">>,
			  {struct,[{<<"outgoingOps">>, ClientOpsJSON},
				   {<<"serverOps">>, ServerOpsJSON}]}}]} ->
		    [ClientDocOp] = to_docops(ClientOpsJSON),
		    [ServerDocOp] = to_docops(ServerOpsJSON),
		    io:format("Before: ~p:~p~n", [ClientDocOp, ServerDocOp]),
		    {St, Ct} = ot:transform(ServerDocOp, ClientDocOp),
		    io:format("After: ~p:~p~n", [Ct, St]),

		    Browser ! {send, 
			       mochijson2:encode(
				 {struct, [{<<"transformed">>, 
					    {struct, [
						      {<<"cdash">>, ops_to_json_format([Ct])},
						      {<<"sdash">>, ops_to_json_format([St])}]}}]})},
		    
		    %% State is unaffected
		    proxy(Browser, State0);
		
		%% Client ops have been received
		{struct, [{<<"client_ops">>,
			   {struct, [{<<"clientVersion">>, ClientClientVersion},
				     {<<"serverVersion">>, ClientServerVersion},
				     {<<"ops">>, ClientOps}]}}]} ->
		    
		    if 
			
			ServerVersion =:= ClientServerVersion ->
			    ClientDocOps = to_docops(ClientOps),
			    State1 = State0#state{
				       resultingops=State0#state.resultingops ++ ClientDocOps,
				       serverversion=State0#state.serverversion + 1
				      },
			    %%io:format("New state: ~p~n", [State1]),
			    Browser ! {send, mochijson2:encode(
					       {struct, [{<<"ack">>, 
							  {struct, [
								    {<<"clientVersion">>, ClientClientVersion },
								    {<<"serverVersion">>, State1#state.serverversion},
								    {<<"applied_ops">>, length(ClientDocOps)}
								   ]}}]})},

			    replace_x(Browser, State1);

			%% TODO: Update difference > 1 (need composition?)
			ClientServerVersion =:= ServerVersion - 1 ->
			    %%io:format("received: ~p~n", [Msg]),
			    io:format("ClientVersion = ServerVersion - 1 : ~p~n", [State0]),

			    %% Transform the client ops to the unknown
			    %% server ops. E.g. if server version is 2, and client
			    %% thinks server version is 1, then there is one operation
			    %% to transform
			    [ClientDocOp] = to_docops(ClientOps),
			    ServerDocOp = lists:nth(length(ServerOps), ServerOps),

			    io:format("SS: Before: ~p:~p~n", [ClientDocOp, ServerDocOp]),
			    {St, Ct} = ot:transform(ServerDocOp, ClientDocOp),
			    io:format("SS: After: ~p:~p~n", [Ct, St]),


			    State1 = State0#state{
				       resultingops=State0#state.resultingops ++ [Ct],
				       serverversion=State0#state.serverversion + 1
				      },

			    %% DocOps = State1#state.resultingops,
%% 			    Document = composer:apply_empty(DocOps),
%% 			    io:format("State: ~p~n", [State1]),
%% 			    io:format("Document: ~p~n", [Document]),
			    
			    proxy(Browser, State1);
			true ->
			    io:format("received: ~p~n", [Msg]),
			    io:format("?? : ~p~n", [State0]),
			    proxy(Browser, State0)
		    end;
		_ ->

		    proxy(Browser, State0)
	    end;
	{append, Browser, NewOps} ->
	    DocOps = State0#state.resultingops,
	    Document = composer:apply_empty(DocOps),
 	    io:format("document: ~p~n", [Document]),
	    State1 = State0#state{
		       resultingops=State0#state.resultingops ++ NewOps,
		       serverversion=State0#state.serverversion + 1
		      },
 	    Browser ! {send, mochijson2:encode(
    			       {struct, [{<<"ops">>, 
    					  {struct, [
    						    {<<"serverVersion">>,  
  						     State1#state.serverversion},
    						    {<<"applied_ops">>,
						     ops_to_json_format(NewOps)}
    						   ]}}]})},
	    proxy(Browser, State1);
	{browser_closed, _Browser} ->
	    ok;
	X -> 
	    io:format("Unknown message: ~p~n", [X]),
	    proxy(Browser, State0)
%%     after 5000 ->
%% 	    DocOps = State0#state.resultingops,
%% 	    Document = composer:apply_empty(DocOps),
%% 	    NewOps = [case length(Document) of
%% 			 0 -> [];
%% 			 _ -> [{ret, length(Document)}]
%% 		     end ++  [{ins, "X"}]],
%% 	    State1 = State0#state{
%% 		       resultingops=State0#state.resultingops ++ NewOps,
%% 		       serverversion=State0#state.serverversion + 1
%% 		      },
%%  	    Browser ! {send, mochijson2:encode(
%%     			       {struct, [{<<"ops">>, 
%%     					  {struct, [
%%     						    {<<"serverVersion">>,  
%%   						     State1#state.serverversion},
%%     						    {<<"applied_ops">>,
%% 						     ops_to_json_format(NewOps)}
%%     						   ]}}]})},
%% 	    proxy(Browser, State1)
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
		 "WebSocket-Origin: null\r\n",
		 "WebSocket-Location: ",
		 "  ws://localhost:1234/websession\r\n\r\n"
		],
	    gen_tcp:send(Socket, Handshake),
	    BrowserPid = self(),
	    Pid = spawn_link(fun() -> proxy(BrowserPid, State) end),
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
	{append, NewOps} ->
	    Pid ! {append, self(), NewOps},
	    loop(Buff, Socket, Pid);
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

	    

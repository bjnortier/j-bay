-module(ot_srv).
-compile(export_all).
-compile(debug_info).


start() ->
    %% State = {ServerVersion, Clients, Doc}
    State = {0, [], []}, 
    spawn(?MODULE, loop, [State]).

loop(State = {ServerVersion, Clients, DocOps}) ->
    receive 
	{add_client, Client} ->
	    Client ! {connect, self()},
	    loop({ServerVersion, Clients ++ [Client], DocOps});
	{msg, From, Msg} ->
	    loop(rcv(From, State, Msg));
	{state} ->
	    io:format("~p State: ~p~n", [self(), State]),
	    loop(State);
	{quit} -> 
	    io:format("~p exiting~n", [self()]),
	    exit(normal);
	Msg ->
	    io:format("~p received unknown msg: ~p~n", [self(), Msg]),
	    loop(State)
    end.


rcv(From, _State = {ServerVersion, Clients, DocOps}, Msg = {Op, Version, LastServerVersion}) ->
    io:format("Server at version ~p received ~p ops: ~p~n", [ServerVersion, Msg, DocOps]),

    %% Determine the 'outgoing' queue against which this received operation
    %% must be transformed before it can be applied locally
    OpsAfterLastReceivedVersion = 
	case LastServerVersion - ServerVersion of
	    0 -> [];
	    _ -> lists:sublist(DocOps, LastServerVersion + 1, length(DocOps) - LastServerVersion)
	end,
		 
    OutgoingQueue = lists:map(fun(DocOp) ->
				      {DocOp, 0}
			      end,
			      OpsAfterLastReceivedVersion),


    {TransformedOp, NewOutgoing} = transform(Op, OutgoingQueue, []),

    io:format("~p ~p:~p~n->~n~p~p~n", [self(), Op, OutgoingQueue, TransformedOp, NewOutgoing]),

    NewServerVersion = ServerVersion + 1,
    NewDocOps = DocOps ++ [TransformedOp],

    %% Acknowledge the sender that the operation has been
    %% applied
    From ! {ack, {Version, NewServerVersion}},

    lists:map(fun(Client) ->
		      case Client of
			  From -> ok;
			  _ ->  Client ! {msg, NewServerVersion, {TransformedOp}}
		      end
	      end,
	      Clients),

    {NewServerVersion, Clients, NewDocOps}.


transform(Op, [Hd|Rest], OutgoingDash) ->
    {OutgoingOp, LocalVersion} = Hd,
    {CDash, SDash} = ot:transform(Op, OutgoingOp),
    transform(CDash, Rest, OutgoingDash ++ [{SDash, LocalVersion}]);
transform(Op, [], OutgoingDash) ->
    {Op, OutgoingDash}.
    

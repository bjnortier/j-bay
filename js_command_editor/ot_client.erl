-module(ot_client).
-compile(export_all).
-compile(debug_info).

start() ->
    %% State = {OutgoingQueue, NextGeneratedVersion, LastServerVersion, Server, Doc}
    State = {[], 0, 0, undefined, []}, 
    spawn(?MODULE, loop, [State]).

loop(State = {OutgoingQueue, NextGeneratedVersion, LastServerVersion, _Server, DocOps}) ->
    receive 
	{connect, NewServer} ->
	    loop({OutgoingQueue, NextGeneratedVersion, LastServerVersion, NewServer, DocOps});
	{generate, Op} ->
	    loop(gen(State, Op));
	{ack, Versions} ->
	    loop(ack(State, Versions));
	{msg, NewServerVersion, Msg} ->
	    loop(rcv(State, NewServerVersion, Msg));
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

gen({OutgoingQueue, NextGeneratedVersion, LastServerVersion, Server, DocOps}, Op) ->
    io:format("~p generating ~p~n", [self(), Op]),

    NewDocOps = DocOps ++ [Op],
    Server ! {msg, self(), {Op, NextGeneratedVersion, LastServerVersion}},

    NewOutgoing = OutgoingQueue ++ [{Op, NextGeneratedVersion}],

    {NewOutgoing, NextGeneratedVersion + 1, LastServerVersion, Server, NewDocOps}.

ack(_State = {OutgoingQueue, NextGeneratedVersion, LastServerVersion, Server, DocOps}, {AckVersion, NewServerVersion}) ->
    %% The server has acknowledges that it has applied the
    %% operation to it's local state. We can remove the outgoing
    %% operations up to this point

    io:format("~p received ack for version ~p~n", [self(), AckVersion]),
    RemainingOutgoing = 
	lists:foldl(fun(Outgoing = {_OutgoingOp, Version}, Acc) ->
			    if 
				Version =< AckVersion ->
				    Acc;
				true -> 
				    Acc ++ [Outgoing]
			    end
		    end,
		    [],
		    OutgoingQueue),
    {RemainingOutgoing, NextGeneratedVersion, NewServerVersion, Server, DocOps}.
    

rcv(_State = {OutgoingQueue, NextGeneratedVersion, LastServerVersion, Server, DocOps},
    NewServerVersion,
    Msg = {ServerOp}) ->

    io:format("~p received ~p:~p~n", [self(), NewServerVersion, Msg]),

    %% Transforms against outgoing
    {NewOutgoing, TransformedServerOp} = transform(OutgoingQueue, ServerOp, []),
    io:format("~p ~p:~p~n->~n~p~p~n", [self(), OutgoingQueue, ServerOp, NewOutgoing, TransformedServerOp]),

    NewDocOps = DocOps ++ [TransformedServerOp],
    {NewOutgoing, NextGeneratedVersion, NewServerVersion, Server, NewDocOps}.

transform([Hd|Rest], ServerOp, OutgoingDash) ->
    {LocalOp, LocalVersion} = Hd,
    {CDash, SDash} = ot:transform(LocalOp, ServerOp),
    transform(Rest, SDash, OutgoingDash ++ [{CDash, LocalVersion}]);
transform([], SDash, OutgoingDash) ->
    {OutgoingDash, SDash}.
    

test() ->
    test3(),
    ok.

test1() ->   
    S = ot_srv:start(),
    C1 = ot_client:start(),
    C2 = ot_client:start(),
    S ! {add_client, C1},
    S ! {add_client, C2},
    timer:sleep(100),

    S ! {state},
    C1 ! {state},
    C2 ! {state},

    io:format("~nServer: ~p Clients: ~p ~p~n", [S, C1, C2]),

    C1 ! {generate, [{insert, "abc"}]},
    C2 ! {generate, [{insert, "xyz"}]},

    timer:sleep(100),
    S ! {state},
    C1 ! {state},
    C2 ! {state},

    timer:sleep(100),
    S ! {quit},
    C1 ! {quit},
    C2 ! {quit},
    ok.


test2() ->   
    S = ot_srv:start(),
    C1 = ot_client:start(),
    C2 = ot_client:start(),
    C3 = ot_client:start(),
    S ! {add_client, C1},
    S ! {add_client, C2},
    S ! {add_client, C3},
    timer:sleep(100),

    io:format("~nServer: ~p Clients: ~p ~p ~p~n", [S, C1, C2, C3]),

    C1 ! {generate, [{insert, "111"}]},
    C2 ! {generate, [{insert, "222"}]},
    C3 ! {generate, [{insert, "333"}]},

    timer:sleep(100),
    S ! {state},
    C1 ! {state},
    C2 ! {state},
    C3 ! {state},

    timer:sleep(100),
    S ! {quit},
    C1 ! {quit},
    C2 ! {quit},
    C3 ! {quit},
    ok.

test3() ->   
    S = ot_srv:start(),
    C1 = ot_client:start(),
    C2 = ot_client:start(),
    S ! {add_client, C1},
    S ! {add_client, C2},
    timer:sleep(100),

    S ! {state},
    C1 ! {state},
    C2 ! {state},

    io:format("~nServer: ~p Clients: ~p ~p~n", [S, C1, C2]),

    C1 ! {generate, [{insert, "abc"}]},

    timer:sleep(100),
    S ! {state},
    C1 ! {state},
    C2 ! {state},

    C1 ! {generate, [{retain, 1},{delete, "b"},{retain, 1}]},
    C2 ! {generate, [{retain, 1},{delete, "b"},{retain, 1}]},

    timer:sleep(100),
    S ! {state},
    C1 ! {state},
    C2 ! {state},


    timer:sleep(100),
    S ! {quit},
    C1 ! {quit},
    C2 ! {quit},
    ok.

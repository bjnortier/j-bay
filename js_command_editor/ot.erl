-module(ot).
-include_lib("eunit/include/eunit.hrl").
%%-define(_assertMatch(X, Y), X =:= Y).
-compile(export_all).

op_skip_size({insert, _}) ->
    0;
op_skip_size({delete, Chars}) ->
    length(Chars);
op_skip_size({retain, N}) ->
    N.


%%
%% The argument indicates how the size of the undesired segment, so the
%% operation should be split up into segments of size length(Op) - X and X
split_op({delete, Chars}, SecondSegmentSize) ->
    Length = length(Chars),
    {{delete, lists:sublist(Chars, 1, Length - SecondSegmentSize)},
     {delete, lists:sublist(Chars, Length - SecondSegmentSize + 1, SecondSegmentSize)}};
split_op({retain, N}, SecondSegmentSize) ->
    {{retain, N - SecondSegmentSize}, {retain, SecondSegmentSize}};
split_op(Component, _) ->
    throw(io_lib:format("split_op called on unsplittable component, ~p", [Component])).

apply_op({insert, Chars}) ->
    {{insert, Chars}, {retain, length(Chars)}};

apply_op({retain, N}) ->
    {{retain, N}, {retain, 0}};

apply_op({delete, Chars}) ->
    {{delete, Chars}, {retain, -length(Chars)}}.


transform(ClientOps, ServerOps) ->
    %%transform(ClientOps, ServerOps, [], [], 0, 0).
    {CDashWithNoOp, SDashWithNoOp} = transform(ClientOps, ServerOps, [], [], 0, 0),
    %% Used foldr here instead of foldl as we're using the faster
    %% list cons function instead of the ++ operator
    {filter_empty(compress(CDashWithNoOp)),
     filter_empty(compress(SDashWithNoOp))}.

transform(ClientOps, ServerOps, CAcc, SAcc, ClientPos, ServerPos) ->
%%     io:format("C: ~p S: ~p~nC': ~p S': ~p Cp: ~p Sp: ~p~n", 
%% 	      [ClientOps, ServerOps, CAcc, SAcc, ClientPos, ServerPos]),
    transform_(ClientOps, ServerOps, CAcc, SAcc, ClientPos, ServerPos).


transform_(ClientOps, [Sop|Rem], CAcc, SAcc, ClientPos, ServerPos) when ClientPos > ServerPos ->
    SkipSize = op_skip_size(Sop),
    if
	ServerPos + SkipSize > ClientPos ->
	    {ServerOp1, ServerOp2} = split_op(Sop, ServerPos + SkipSize - ClientPos),
	    transform(ClientOps, [ServerOp1 | [ ServerOp2 | Rem] ], CAcc, SAcc, ClientPos, ServerPos);
	true ->
	    {SopT, CopT} = apply_op(Sop),
	    transform(ClientOps, Rem, CAcc ++ [CopT], SAcc ++ [SopT], ClientPos, ServerPos + SkipSize)
    end;
transform_([Cop|Rem], ServerOps, CAcc, SAcc, ClientPos, ServerPos) ->
    SkipSize = op_skip_size(Cop),
    {CopT, SopT} = apply_op(Cop),
    transform(Rem, ServerOps, CAcc ++ [CopT], SAcc ++ [SopT], ClientPos + SkipSize, ServerPos);
transform_([], [Sop|Rem], CAcc, SAcc, ClientPos, ServerPos) ->
    SkipSize = op_skip_size(Sop),
    {SopT, CopT} = apply_op(Sop),
    transform([], Rem, CAcc ++ [CopT], SAcc ++ [SopT], ClientPos, ServerPos + SkipSize);
%% No more components
transform_([], [], Cdash, Sdash, _, _) ->
    {Cdash, Sdash}.
    

compress(Components) ->
    compress_retains(compress_deletes(Components)).


compress_deletes([]) ->
    [];
compress_deletes([H|Rest]) ->
    compress_deletes(H, Rest, []).

%% The delete-retain(-N) pairs are compressed first, to void interference
%% with other retains within the list of components. These pair have a higher
%% priority than the cancellation of the retains

compress_deletes(Hd, [], Acc) ->
    Acc ++ [Hd];
%% TODO: What about when -N is -1 and length(Chars) == 2?
compress_deletes({retain, N}, [{delete, Chars}|Rest], Acc) when -N =:= length(Chars) ->
    compress_deletes({retain, 0}, Rest, Acc);
compress_deletes({delete, Chars}, [{retain, N}|Rest], Acc) when -N =:= length(Chars) ->
    compress_deletes({retain, 0}, Rest, Acc);
compress_deletes(Hd, [Next |Rest], Acc) ->
    compress_deletes(Next, Rest, Acc ++ [Hd]).

compress_retains([]) -> 
    [];
compress_retains([H|Rest]) ->
    compress_retains(H, Rest, []).

compress_retains(Hd, [], Acc) ->
    Acc ++ [Hd];
%% TODO: What about when -N is -1 and length(Chars) == 2?
compress_retains({retain, N}, [{delete, Chars}|Rest], Acc) when -N =:= length(Chars) ->
    compress_retains({retain, 0}, Rest, Acc);
compress_retains({delete, Chars}, [{retain, N}|Rest], Acc) when -N =:= length(Chars) ->
    compress_retains({retain, 0}, Rest, Acc);
compress_retains({retain, N}, [{retain,M} |Rest], Acc) ->
    compress_retains({retain, N+M}, Rest, Acc);
compress_retains(Hd, [Next |Rest], Acc) ->
    compress_retains(Next, Rest, Acc ++ [Hd]).
    
filter_empty(Components) ->
    F = fun(Op, Acc) -> 
		case Op of
		    {retain, 0} -> Acc;
		    _ -> [Op|Acc]
		end
	end,
    lists:foldr(F, [], Components).


compress_test_() ->
    [
     ?_assertMatch([],compress([])),
     ?_assertMatch([{insert, "abc"},  {retain, 0}], compress([{insert, "abc"}, {retain, 1}, {retain, -1}])),
     ?_assertMatch([{retain,1},{delete,"b"}, {retain, 1}],compress([{retain,1},{delete,"b"},{retain,2},{retain,-1}])),
     ?_assertMatch([{retain,2}], compress([{retain,1},{delete,"b"},{retain,-1},{retain,1}])),
     ?_assertMatch([{retain,1}], compress([{retain,-1},{delete,"b"},{retain,1}])),
     ?_assertMatch([{retain, 2}], 
		   compress([{retain,1}, 
			     {retain,0},
			     {delete,"X"},
			     {retain,-1},
			     {retain,1},
			     {retain,0}])),
     ?_assertMatch([{retain, 2}],
		   compress([{retain,0},
			     {retain,1},
			     {retain,-1},
			     {delete,"X"},
			     {retain,0},
			     {retain,1}]))
    ].
    
filter_empty_test_() ->
    [
     ?_assertMatch([{insert, "abc"}, {retain, 2}], 
		   filter_empty([{insert, "abc"}, {retain, 0}, {retain, 2}]))
    ].

transform_1_test_() ->
    ClientComponents = [{insert, "abc"}],
    ServerComponents = [{insert, "def"}],
    [
     ?_assertMatch(
	{[{insert, "abc"}, {retain, 3}], [{retain, 3}, {insert, "def"}]},
	transform(ClientComponents, ServerComponents))
    ].

test_transform_2() ->
    ClientComponents = [{insert, "xyz"}, {retain, 3}],
    ServerComponents = [{retain, 3}, {insert, "def"}],
    [
     ?_assertMatch(
	{[{insert, "xyz"}, {retain, 3}, {retain, 3}],
	 [{retain, 3}, {retain, 3}, {insert, "def"}]},
	transform(ClientComponents, ServerComponents))
    ].

test_transform_2b() ->
    ClientComponents = [{insert, "abc"}, {retain, 1}, {insert, "def"}],
    ServerComponents = [{retain, 1}],
    [
     ?_assertMatch(
	{[{insert, "abc"}, {retain, 1}, {insert, "def"}],
	 [{retain, 3}, {retain, 1}, {retain, 3}]},
	transform(ClientComponents, ServerComponents))
    ].

test_transform_2c() ->
    ClientComponents = [{insert, "xyz"}, {retain, 3}],
    ServerComponents = [{retain, 3}, {insert, "def"}],
    [
     ?_assertMatch(
	{[{insert, "xyz"}, {retain, 3}, {retain, 3}],
	 [{retain, 3}, {retain, 3}, {insert, "def"}]},
	transform(ClientComponents, ServerComponents))
    ].


test_transform_3() ->
    ClientComponents = [{retain, 1}, {delete, "b"}, {retain, 1}],
    ServerComponents = [{retain, 2}, {delete, "c"}],
    [
     ?_assertMatch(
	{[{retain, 1}, {delete, "b"}, {retain, 1}, {retain, -1}],
	 [{retain, 2}, {retain, -1}, {delete, "c"}]},
	transform(ClientComponents, ServerComponents))
    ].


test_transform_4() ->
    ClientComponents = [{insert, "def"}],
    ServerComponents = [{insert, "abc"}],
    [
     ?_assertMatch(
	{[{insert, "def"}, {retain, 3}],
	 [{retain, 3}, {insert, "abc"}]},
	transform(ClientComponents, ServerComponents))
    ].

transform_5_test_() ->
    ClientComponents = [{delete, "b"}],
    ServerComponents = [{delete, "b"}],
    [
     ?_assertMatch(
	{[], []}, 
	transform(ClientComponents, ServerComponents))
    ].

transform_7_test_() ->
    ClientComponents = [{delete, "abc"}, {retain, 3}],
    ServerComponents = [{retain, 3}, {insert, "!"}, {retain, 3}],
    [
     ?_assertMatch({[{delete, "abc"}, {retain, 4}],
		    [{insert, "!"}, {retain, 3}]},
		   transform(ClientComponents, ServerComponents))
    ].

transform_8_test_() ->
    ClientComponents = [{delete, "abc"}, {retain, 3}],
    ServerComponents = [{retain, 3}, {insert, "!"}, {retain, 3}],
    [
     ?_assertMatch({[{delete, "abc"}, {retain, 4}],
		    [{insert, "!"}, {retain, 3}]},
		   transform(ClientComponents, ServerComponents))
    ].


transform_9_test_() ->
    ClientComponents = [{retain, 3}, {insert, "X"}, {retain, 3}],
    ServerComponents = [{retain, 2}, {delete, "cd"}, {retain, 2}],
    [
     ?_assertMatch({[{retain, 2}, {insert, "X"}, {retain, 2}],
		    [{retain, 2}, {delete, "c"}, {retain, 1}, {delete, "d"}, {retain, 2}]},
		   transform(ClientComponents, ServerComponents))
    ].

transform_10_test_() ->
    ClientComponents = [{retain, 1}, {delete, "X"}, {retain, 1}],
    ServerComponents = [{retain, 1}, {delete, "X"}, {retain, 1}],
    [
     ?_assertMatch({[{retain, 2}],
		    [{retain, 2}]},
		   transform(ClientComponents, ServerComponents))
    ].

segment_test_() ->
     [
      ?_assertMatch({{retain, 2}, {retain, 1}}, split_op({retain, 3}, 1)),
      ?_assertMatch({{retain, 0}, {retain, 3}}, split_op({retain, 3}, 3)),
      ?_assertMatch({{delete, "c"}, {delete, "d"}}, split_op({delete, "cd"}, 1)),
      ?_assertMatch({{delete, "d"}, {delete, "ef"}}, split_op({delete, "def"}, 2))
     ].



-module(composer).
-compile(export_all).

apply_empty(Ops) ->
    apply_empty("", Ops).

apply_empty(Doc, [H|Rest]) ->
    NextDoc = apply_op(Doc, H), 
    apply_empty(NextDoc, Rest);
apply_empty(Doc, []) ->
    Doc.

apply_op(Doc, Op) ->
    apply_op(Doc, "", Op, 1).

apply_op(Doc, NewDoc, [{ins, Str}|Rest], Cursor) ->
    apply_op(Doc, NewDoc ++ Str, Rest, Cursor);
apply_op(Doc, NewDoc, [{del, Str}|Rest], Cursor) ->
    apply_op(Doc, NewDoc, Rest, Cursor + length(Str));
apply_op(Doc, NewDoc, [{ret, N}|Rest], Cursor) ->
    apply_op(Doc, NewDoc ++ string:substr(Doc, Cursor, N), Rest, Cursor + N);
apply_op(_Doc, NewDoc, [], _) ->
    NewDoc.

test() ->
    test1(),
    test2(),
    test3(),
    ok.

test1() ->
    Ops = [[{ins, "a"}]],
    "a" = apply_empty(Ops).

test2() ->
    Ops = [[{ins, "abc"}],
	   [{ret, 1}, {del, "bc"}]],
    "a" = apply_empty(Ops).

test3() ->
    Ops = [[{ins, "1234"}],
	   [{ret, 2},{ins, "X"},{ret, 2}]],
    "12X34" = apply_empty(Ops).



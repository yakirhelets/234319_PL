compress([], []).
compress([X,X|Xs], L):-
    compress([X|Xs], L), !.
compress([X|Xs], [X|L]):-
    compress(Xs, L).

range(X, X, [X]).
range(X, Y, [X|Xs]):-
 	X1 is X+1,
 	range(X1,Y,Xs), !.

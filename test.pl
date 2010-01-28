% Comment

sum(zero, X, X).
sum(s(X), Y, s(Z)) :- sum(X, Y, Z).

conn(berlin, moscow).
conn(berlin, bonn).
conn(moscow, paris).

path(X, X).
path(X, Y) :-
	conn(X, Z),
	path(Z, Y).

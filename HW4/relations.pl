parent(kim,holly).
parent(margaret,kim).
parent(margaret,kent).
parent(esther,margaret).
parent(herbert,margaret).
parent(herbert,jean).
grandparent(GP,GC) :-
    parent(GP,C) , parent(C,GC).
ggrandparent(GGP,GGC) :-
    grandparent(GGP, P) , parent(P, GGC).

ancestor(X,Y) :- parent(X,Y) .
ancestor(X,Y) :-
    parent(Z,Y) , ancestor(X,Z).

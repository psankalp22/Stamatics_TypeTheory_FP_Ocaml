/* === Assignment 1: Implementation of Sets as Lists with no duplicates === */

 :- discontiguous mem/2.

/* === Memership Function: mem === */

mem(X, [X|_])               :-      !.
mem(X, [_|R])               :-      mem(X, R).

/* === Non-Membership Function: notmem === */

notmem(_, [])               :-      !.
notmem(X, [Y|R])            :-      notmem(X, R), X \= Y.

/* === Delete Function: del === */

del(_X, [], [])             :-      !.
del(X, [X|R], Z)            :-      del(X, R, Z).
del(X, [Y|R], [Y|Z])        :-      del(X, R, Z).

/* === Remove Duplicates Function: remdups === */

remdups([], [])             :-      !.
remdups([X|R], [X|Z])       :-      del(X, R, L), remdups(L, Z).

/* === Union Function: unionI === */

unionI([], [], [])          :-      !.
unionI([], [X|R], [X|Z])    :-      unionI([], R, L), remdups(L, Z).
unionI(L1, L2, L4)          :-      unionI([], L1, L3), unionI([], L2, L3), remdups(L3, L4).

/* === Append Function: append === */

append([], L, L)            :-      !.
append([X|R], L, [X|Z])     :-      append(R, L, Z).

/* === Mapcons Function: mapcons === */

mapcons(_, [], [])              :-      !.
mapcons(X, [Y|R], [[X|Y] | Z])  :-      mapcons(X, R, Z).

/* === Power Function: powerI === */

powerI([], [ [] ])          :-      !.
powerI([X|R], P)            :-      powerI(R, P1), mapcons(X, P1, P2), append(P2, P1, P).

/* === Intersection Function: interI === */

interI(_, [], [])           :-      !.
interI(S1, [X|R], [X|Z])    :-      interI(S1, R, Z), mem(X, S1).
interI(S1, [X|R], Z)        :-      interI(S1, R, Z), notmem(X, S1).

/* === Difference Function: diffI === */

diffI([], _, [])            :-      !.
diffI([X|R], S2, [X|Z])     :-      diffI(R, S2, Z), interI([X|R], S2, L), notmem(X, L).
diffI([X|R], S2, Z)         :-      diffI(R, S2, Z), interI([X|R], S2, L), mem(X, L).

/* === Inverse Relation: inv === */

mem( (Y,X), inv(R) )        :-      mem( (X,Y), R ).

/* === Reflexive Closure: refclos === */

mem( (X,X), refclos(_, S) )    :-      mem( X, S ).
mem( (X,Y), refclos(R, _) )    :-      mem( (X,Y), R ).

/* === Symmetric Closure: symclos === */

mem( (X,Y), symclos(R, _) )    :-      mem( (X,Y), R).
mem( (X,Y), symclos(R, _) )    :-      mem( (X,Y), inv(R) ).

/* === Transitive Closure: transclos === */

mem( (X,Y), transclos(R, _) )   :-      mem( (X,Y), R).
mem( (X,Z), transclos(R, S) )   :-      mem( (X,Y), R), mem( (Y,Z), transclos(R, S) ).

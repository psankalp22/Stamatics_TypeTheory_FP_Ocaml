/*=== Lecture 1: Sets === */

 :- discontiguous mem/2.

/* membership */
 mem(_, [])             :-      fail.
 mem(X, [X|_])          :-      !.
 mem(X, [_|R])          :-      mem(X, R).

/* subset */
 subset([], [])         :-      !.
 subset([], [_|_])      :-      !.
 subset([X|R], L)       :-      mem(X, L), subset(R, L).

/* equal sets */
 eqset(L1, L2)          :-      subset(L1, L2), subset(L2, L1).

/* union */
 mem(X, union(S1, _))   :-      mem(X, S1).
 mem(X, union(_, S2))   :-      mem(X, S2).

/* intersection */
 mem(X, inter(S1, S2))  :-      mem(X, S1), mem(X, S2).

/* difference */
 mem(X, diff(S1, S2))   :-      mem(X, S1), \+ mem(X, S2).

/* complement */
 mem(X, compl(S))       :-      \+ mem(X, S).

/* powerset */
 mem(X, power(S))       :-      subset(X, S).

/* cartesian product */
 mem( (X,Y) , cart(S1, S2) )    :-      mem(X, S1), mem(Y, S2).

/* identity relation */
 mem( (X,X) , id(S) )           :-      mem(X, S).

/* relational composition */
 mem( (X,Z) , comp(R1, R2) )    :-      mem( (X,Y) , R1 ),
                                        mem( (Y,Z) , R2 ).

/* inverse relation */
 mem( (Y,X) , inv(R) )          :-      mem( (X,Y) , R ).

/* reflexive closure */
 mem( (X,X) , refclos(_, S) )   :-      mem(X, S), !.
 mem( (X,Y) , refclos(R, _) )   :-      mem( (X,Y) , R), !.

/* symmetric closure */
 mem( (X,Y) , symclos(R) )      :-      mem( (X,Y) , R), !.
 mem( (X,Y) , symclos(R) )      :-      mem( (X,Y) , inv(R) ), !.

/* transitive closure */

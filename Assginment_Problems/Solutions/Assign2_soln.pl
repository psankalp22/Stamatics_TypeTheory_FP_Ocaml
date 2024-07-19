/* === Assignment 2: A Type Checker === */

/* base value: boolean */
hastype(true, boolT)    :-  !.
hastype(false, boolT)   :-  !.

/* base value: integer */
hastype(X, intT)        :-  integer(X).

/* arithmetic operators */
hastype( plus(E1,E2), intT) :-  hastype(E1, intT), hastype(E2, intT).
hastype( mult(E1,E2), intT) :-  hastype(E1, intT), hastype(E2, intT).

/* logical operators */
hastype( and(E1,E2), boolT) :-  hastype(E1, boolT), hastype(E2, boolT).
hastype( or(E1,E2), boolT)  :-  hastype(E1, boolT), hastype(E2, boolT).
hastype( not(E1,E2), boolT) :-  hastype(E1, boolT), hastype(E2, boolT).

/* relational operators */
hastype( eq(E1,E2), boolT)  :-  hastype(E1, boolT), hastype(E2, boolT).
hastype( eq(E1,E2), intT)   :-  hastype(E1, intT), hastype(E2, intT).

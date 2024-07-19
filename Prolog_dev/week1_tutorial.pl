/* === Tests for L1.pl === */

:- use_module(library(plunit)).
:- consult('L1.pl').

/* === Test suite for mem/2 predicate === */

:- begin_tests(mem_tests).

test(mem_1)     :-      mem(1, [1, 2, 3, 4, 5]).
test(mem_2)     :-      mem(1, [5, 4, 3, 2, 1]).
test(mem_3)     :-      mem(1, [5, 4, 1, 2, 3]).
test(mem_4)     :-      \+ mem(1, [2, 3, 4, 5]).
test(mem_5)     :-      \+ mem(1, []).
test(mem_6)     :-      \+ mem([], []).

:- end_tests(mem_tests).

/* === Test suite for subset/2 predicate === */

:- begin_tests(subset_tests).

test(subset_1)  :-      subset([], []).
test(subset_2)  :-      \+ subset(1, []).
test(subset_3)  :-      \+ subset([1], []).
test(subset_4)  :-      \+ subset([1], [2, 3]).
test(subset_5)  :-      subset([1], [1, 2, 3]).
test(subset_6)  :-      subset([2], [1, 2, 3]).
test(subset_7)  :-      subset([3], [1, 2, 3]).
test(subset_8)  :-      subset([1, 2], [1, 2, 3]).
test(subset_9)  :-      subset([2, 1], [1, 2, 3]).
test(subset_10)  :-      subset([1, 2, 3], [1, 2, 3]).
test(subset_11)  :-      subset([3, 1, 2], [1, 2, 3]).
test(subset_12)  :-      \+ subset([1, 2, 3, 4], [1, 2, 3]).

:- end_tests(subset_tests).

/* === Test suite for eqset/2 predicate === */

:- begin_tests(eqset_tests).

test(eqset_1)   :-      eqset([], []).
test(eqset_2)   :-      \+  eqset([], [1]).
test(eqset_3)   :-      \+  eqset([1], []).
test(eqset_4)   :-      eqset([1, 2], [2, 1]).
test(eqset_5)   :-      eqset([1, 2, 3], [1, 2, 3]).

:- end_tests(eqset_tests).

/* === Test suite for union/2 predicate === */

:- begin_tests(union_tests).

:- end_tests(union_tests).


/* === Test suite for inter/2 predicate === */

:- begin_tests(inter_tests).

:- end_tests(inter_tests).

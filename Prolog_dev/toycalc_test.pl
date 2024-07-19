/* === Tests for L2.pl === */

:- use_module(library(plunit)).
:- consult('L2.pl').

/* === Test suite for calculate/2 predicate === */

:- begin_tests(calculate_tests).

test(calc_num)          :-          calculate(numeral(0), numeral(0)).

test(calc_plus)         :-          calculate(
                                    plus(numeral(3), numeral(4)), 
                                    numeral(7)
                                ).

test(calc_times)        :-          calculate(
                                    times(numeral(3), numeral(4)), 
                                    numeral(12)
                                ).

test(calc_times_zero)   :-          calculate(
                                    times(numeral(3), numeral(0)), 
                                    numeral(0)
                                ).

:- end_tests(calculate_tests).

/* === Test suite for compile/2 predicate === */

:- begin_tests(compiler_tests).

test(load_operation)    :-      compile(numeral(1), [ldop(1)]).

test(plus_operation)    :-      compile(
                                    plus(numeral(3), numeral(4)),
                                    [ldop(3), ldop(4), plusop]
                                ).

test(times_operation)   :-      compile(
                                    times(numeral(3), numeral(4)),
                                    [ldop(3), ldop(4), timesop]
                                ).

:- end_tests(compiler_tests).

/* === Test suite for stackmc/3 predicate === */

:- begin_tests(stackmc_tests).

test(empty_stack)       :-      stackmc([42], [], 42).
test(load_stack)        :-      stackmc([], [ldop(42)], 42).
test(plus_stack)        :-      stackmc([30, 12], [plusop], 42).
test(times_stack)       :-      stackmc([3, 4], [timesop], 12).

:- end_tests(stackmc_tests).

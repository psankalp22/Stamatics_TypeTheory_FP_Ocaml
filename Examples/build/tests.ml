(* importing *)
open OUnit2
open Expression
open Operator

let operator_tests = "test suite for operators" >::: [
    "not"   >:: (fun _ -> assert_equal (B false) (apply_un_op(Not, B false)));
]

let _ = run_test_tt_main operator_tests



(* == type definition of values == *)

type value = 
    N of int
|   B of bool
;;

(* == type definition of operators == *)

type unary_op = Not ;;
type binary_op = Plus | Times | And | Or | Eq | Gt ;;
type ternary_op = IfTE ;;

(* == type definition of expressions == *)

type exp =
    Val of value
|   Var of string
|   Un  of unary_op * exp
|   Bin of binary_op * exp * exp
|   Ter of ternary_op * exp * exp * exp
;;

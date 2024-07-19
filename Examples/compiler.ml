(* importing *)

open Expression

(* type definition of operation codes *)

type opcode =
    LDN of int | LDB of bool
|   LOOKUP of string
|   UNOP of unary_op
|   BINOP of binary_op
|   TEROP of ternary_op
;;

(* compile functions *)

let compile_val value = match value with
    N n ->  [ LDN n ]
|   B b ->  [ LDB b ]
;;

let compile_var x = [ LOOKUP x ] ;;

let rec compile_exp e = match e with
    Val v                   ->  compile_val v
|   Var x                   ->  compile_var x
|   Un  (op, e1)            ->  (compile_exp e1) @ [ UNOP op ]
|   Bin (op, e1, e2)        ->  (compile_exp e1) @ (compile_exp e2) @ [ BINOP op ]
|   Ter (op, e1, e2, e3)    ->  (compile_exp e1) @ (compile_exp e2) @ (compile_exp e3) @ [ TEROP op ]
;;

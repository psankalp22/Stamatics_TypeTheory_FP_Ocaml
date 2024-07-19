(* import *)

open Compiler
open Expression
open Operator

(* stuck exception *)

exception Stuck of ((string -> value) * value list * opcode list)

(* stack machine *)

let rec stkmc (g, s, c) = match s, c with
    v :: _, [ ]         ->  v
|   s, (LDN n) :: c'    ->  stkmc (g, ((N n) :: s), c') 
|   s, (LDB b) :: c'    ->  stkmc (g, ((B b) :: s), c')
|   s, (LOOKUP x) :: c' ->  stkmc (g, ((g x) :: s), c')

|   v1 :: s',               (UNOP op)  :: c'    ->  stkmc (g, (apply_un_op(op, v1))         :: s', c')
|   v2 :: v1 :: s',         (BINOP op) :: c'    ->  stkmc (g, (apply_bin_op(op, v1, v2))    :: s', c')
|   v3 :: v2 :: v1 :: s',   (TEROP op) :: c'    ->  stkmc (g, (apply_ter_op(op, v1, v2, v3)):: s', c')

|   _, _    ->  raise (Stuck (g, s, c))
;;

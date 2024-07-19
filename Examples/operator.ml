(* importing *)
open Expression

(* custom exceptions *)
exception InvalidOperator of exp ;;

(* mapping operators to function *)

let apply_un_op (op, v1) = match (op, v1) with
    (Not, B b)  ->  B (not b)
|   _   ->  raise (InvalidOperator(Un (op, Val v1)))
;;

let apply_bin_op (op, v1, v2) = match (op, v1, v2) with
    (Plus, N n1, N n2)  ->  N (n1 + n2)
|   (Times, N n1, N n2) ->  N (n1 * n2)
|   (And, B b1, B b2)   ->  B (b1 && b2)
|   (Or, B b1, B b2)    ->  B (b1 || b2)
|   (Eq, N n1, N n2)    ->  B (n1 = n2)
|   (Gt, N n1, N n2)    ->  B (n1 > n2)
|   _   ->  raise (InvalidOperator(Bin (op, Val v1, Val v2)))
;;

let apply_ter_op (op, v1, v2, v3) = match (op, v1, v2, v3) with
    (IfTE, B b, v1, v2) ->  if b then v1 else v2
|   _   ->  raise (InvalidOperator(Ter (op, Val v1, Val v2, Val v3)))
;;

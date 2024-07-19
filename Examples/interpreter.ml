(* importing *)

#use "operator.ml";;

(* interpreter for expressions: eval function *)

let rec eval e rho = match e with
    Val a                   ->  a
|   Var x                   ->  rho x

|   Un  (op, e1)            ->  let v1 = (eval e1 rho)
                                in apply_un_op(op, v1)

|   Bin (op, e1, e2)        ->  let v1 = (eval e1 rho)
                                and v2 = (eval e2 rho)
                                in apply_bin_op (op, v1, v2)

|   Ter (op, e1, e2, e3)    ->  let v1 = (eval e1 rho)
                                and v2 = (eval e2 rho)
                                and v3 = (eval e3 rho)
                                in apply_ter_op (op, v1, v2, v3)
;;

(* importing *)

#use "expression.ml";;
let max3 a b c = max a (max b c)

(* function to measure height of expression tree *)

let rec ht e = match e with
    Val _   |   Var _   ->  0
|   Un  (_, e1)         ->  1 + (ht e1)
|   Bin (_, e1, e2)     ->  1 + (max (ht e1) (ht e2))
|   Ter (_, e1, e2, e3) ->  1 + (max3 (ht e1) (ht e2) (ht e3))
;;

(* function to measure size of expression tree *)

let rec size e = match e with
    Val _   |   Var _   ->  1
|   Un  (_, e1)         ->  1 + (size e1)
|   Bin (_, e1, e2)     ->  1 + (size e1) + (size e2)
|   Ter (_, e1, e2, e3) ->  1 + (size e1) + (size e2) + (size e3)
;;

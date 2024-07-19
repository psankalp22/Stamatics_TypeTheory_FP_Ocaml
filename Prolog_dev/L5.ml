(* === Lecture 5: Inductively Defined Types === *)

(* peano style natural numbers *)
type nat = Z | S of nat;;

(* custom named numbers *)
let zero    =   Z;;
let one     =   S Z;;
let two     =   S (S Z);;
let three   =   S (S (S Z));;

(* conversion function *)
let rec nat2int n = match n with
    Z   ->  0                       (* I(Z)     = 0         *)
|   S x ->  1 + (nat2int x)         (* I(S(x))  = 1 + I(x)  *)
;;

(* recursive operations *)
let rec addnat m n = match m with
    Z   ->  n
|   S x ->  S (addnat x n)
;;

let rec multnat m n = match m with
    Z   ->  Z
|   S x ->  addnat n (multnat x n)
;;

let rec expnat m n = match n with
    Z   ->  (S Z)
|   S x ->  multnat m (expnat m x)
;;

(* === Lists === *)

open List;;

(*  built-in definition of lists is something like
    type 'a list = Nil | Cons of 'a * ('a list)
*)

(* === definition of length
 let rec length l = match l with
     [ ]     ->      0
 |   _ :: xs ->      1 + (length xs)
;;
=== *)

(* === definition of append
let rec append l1 l2 = match l1 with
    [ ]     ->      l2
|   x :: xs ->      (append xs l2)
;;
=== *)

(* custom reverse function -- inefficient *)
let rec rev_inefficient s = match s with
    [ ]     ->      [ ]
|   x :: xs ->      (rev_inefficient xs) @ [x]
;;

(* custom reverse function -- efficient *)
let rev_efficient s =
    let rec rev2 s1 s2 = match s1 with
        [ ]     ->  s2
    |   x :: xs ->  rev2 xs (x :: s2)
    in
        rev2 s [ ]
;;

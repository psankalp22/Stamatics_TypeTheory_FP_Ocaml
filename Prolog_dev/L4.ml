(* === Lecture 4: Introduction to OCaml Programming === *)

(* defining custom types *)
type myBool = T | F;;

(* conversion functions *)
let myBool2bool b = match b with
    T   ->  true
|   F   ->  false
;;

let bool2myBool b = match b with
    true    ->  T
|   false   ->  F
;;

(* custom logical operators *)
let myNot b = match b with
    T   ->  F
|   F   ->  T
;;

let myAnd b1 b2 = match b1 with
    T   ->  b2
|   F   ->  F
;;

let myOr b1 b2 = match b1 with
    T   ->  T
|   F   ->  b2
;;

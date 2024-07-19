(* Token types for the toy calculator *)
type token =
  | Identifier of string
  | Keyword of string
  | Boolean of string
  | Int of int
  | String of string
  | Plus
  | Minus
  | Times
  | Divide
  | Equal
  | NotEqual
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | And
  | Or
  | Not
  | If
  | Then
  | Else
  | LParen
  | RParen
  | Comma
  | EOF

(* Keywords and boolean constants *)
let keywords = ["if"; "then"; "else"; "true"; "false"]
let boolean_operators = ["and"; "or"; "not"]

(* Function to check if a string is a keyword *)
let is_keyword s = List.mem s keywords

(* Function to check if a string is a boolean operator *)
let is_boolean_operator s = List.mem s boolean_operators

(* Function to tokenize an input string *)
let tokenize input =
  let length = String.length input in
  let rec aux pos tokens =
    if pos >= length then List.rev (EOF :: tokens)
    else
      let c = input.[pos] in
      if c = ' ' || c = '\n' || c = '\t' then aux (pos + 1) tokens
      else if c >= '0' && c <= '9' then
        let start = pos in
        let rec find_end p =
          if p < length && input.[p] >= '0' && input.[p] <= '9' then find_end (p + 1) else p
        in
        let end_pos = find_end pos in
        let number = int_of_string (String.sub input start (end_pos - start)) in
        aux end_pos (Int number :: tokens)
      else if c >= 'a' && c <= 'z' || c = '_' then
        let start = pos in
        let rec find_end p =
          if p < length && (input.[p] >= 'a' && input.[p] <= 'z' || input.[p] >= '0' && input.[p] <= '9' || input.[p] = '\'' || input.[p] = '_') then find_end (p + 1) else p
        in
        let end_pos = find_end pos in
        let identifier = String.sub input start (end_pos - start) in
        if is_keyword identifier then aux end_pos (Keyword identifier :: tokens)
        else Identifier identifier :: aux end_pos tokens
      else
        match c with
        | '+' -> aux (pos + 1) (Plus :: tokens)
        | '-' -> aux (pos + 1) (Minus :: tokens)
        | '*' -> aux (pos + 1) (Times :: tokens)
        | '/' -> aux (pos + 1) (Divide :: tokens)
        | '=' -> aux (pos + 1) (Equal :: tokens)
        | '<' -> aux (pos + 1) (LessThan :: tokens)
        | '>' -> aux (pos + 1) (GreaterThan :: tokens)
        | '(' -> aux (pos + 1) (LParen :: tokens)
        | ')' -> aux (pos + 1) (RParen :: tokens)
        | ',' -> aux (pos + 1) (Comma :: tokens)
        | _ -> failwith ("Unexpected character: " ^ String.make 1 c)
  in
  aux 0 []

(* Test cases *)
(*
let () =
  let input = "if x < 10 then y = 5 else z = 3" in
  let tokens = tokenize input in
  List.iter (function
    | Identifier s -> Printf.printf "Identifier: %s\n" s
    | Keyword s -> Printf.printf "Keyword: %s\n" s
    | Int n -> Printf.printf "Int: %d\n" n
    | Plus -> Printf.printf "Plus\n"
    | Minus -> Printf.printf "Minus\n"
    | Times -> Printf.printf "Times\n"
    | Divide -> Printf.printf "Divide\n"
    | Equal -> Printf.printf "Equal\n"
    | LessThan -> Printf.printf "LessThan\n"
    | GreaterThan -> Printf.printf "GreaterThan\n"
    | LParen -> Printf.printf "LParen\n"
    | RParen -> Printf.printf "RParen\n"
    | Comma -> Printf.printf "Comma\n"
    | EOF -> Printf.printf "EOF\n"
    | _ -> ()) tokens
*)

(* Additional test case examples:
let () =
  let inputs = [
    "if a then b else c";
    "x = 42 + y";
    "true and false";
    "z = (x * 2) / (y - 1)";
    "let my_var = 10 in my_var + 5";
  ] in
  List.iter (fun input ->
    Printf.printf "Input: %s\n" input;
    let tokens = tokenize input in
    List.iter (function
      | Identifier s -> Printf.printf "Identifier: %s\n" s
      | Keyword s -> Printf.printf "Keyword: %s\n" s
      | Int n -> Printf.printf "Int: %d\n" n
      | Plus -> Printf.printf "Plus\n"
      | Minus -> Printf.printf "Minus\n"
      | Times -> Printf.printf "Times\n"
      | Divide -> Printf.printf "Divide\n"
      | Equal -> Printf.printf "Equal\n"
      | LessThan -> Printf.printf "LessThan\n"
      | GreaterThan -> Printf.printf "GreaterThan\n"
      | LParen -> Printf.printf "LParen\n"
      | RParen -> Printf.printf "RParen\n"
      | Comma -> Printf.printf "Comma\n"
      | EOF -> Printf.printf "EOF\n"
      | _ -> ()) tokens;
    Printf.printf "\n"
  ) inputs
*)

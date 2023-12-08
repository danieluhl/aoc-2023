open Core

(* let lines = Read.read_file "inputs/day05.txt" *)
let lines = Read.read_file "inputs/test.txt"

(* let split_take_nth str nth = *)
(*   match List.nth (String.split str ~on:':') nth with *)
(* ;; *)

let () = Fmt.pr "%d\n" (List.length lines)

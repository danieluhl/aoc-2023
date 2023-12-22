open Core

(* let lines = Read.read_file "inputs/day08.txt" *)
let lines = Read.read_file "inputs/test.txt"

let get_nth l n =
  match List.nth l n with
  | None -> ""
  | Some x -> x
;;

let split_take_nth str nth del = get_nth (String.split str ~on:del) nth

(*
   sort the lines
   index is the multiplier
   add them all up

   sort:
   get rank for each and compare
   if same, iterate through cards to find the winner
   if different, take the highest

   get card rank: check for each scenario in turn
   5 of a kind
   4 of a kind
   full house
   3 of a kind
   two pair
   pair
   none
     {number: count}
     - if 1 key: 5 of a kind
     - if 2 keys: check for 4 of a kind, full house
     - if 3 keys: check for 3 of a kind or 2 pair
     - if 4 keys: pair
     - if 5 keys: none
*)

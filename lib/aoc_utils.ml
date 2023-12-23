open Core
(* let rec concat_lines acc = *)
(*   match acc with *)
(*   | [] -> "" *)
(*   | h :: t -> h ^ "\n" ^ concat_lines t *)

let read_file filename =
  let file = In_channel.create filename in
  let rec read_lines acc =
    try
      match In_channel.input_line file with
      | Some line -> read_lines (line :: acc)
      | None -> List.rev acc
    with
    | End_of_file -> List.rev acc
  in
  let lines = read_lines [] in
  In_channel.close file;
  lines
;;

(* Get the nth element from a list of strings *)
let split_take_nth str nth del =
  match List.nth (String.split str ~on:del) nth with
  | None -> ""
  | Some x -> x
;;

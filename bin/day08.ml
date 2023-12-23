open Core

let lines = Aoc_utils.read_file "inputs/day08.txt"
(* let lines = Aoc_utils.read_file "inputs/test.txt" *)

(*
   Put everything in a hash table

   keep track of the directions, increment mod size to get next
   - maybe use sequence for this?
   - or we could just build a stack and pop them off then reload once empty

   go to the first item in the hash table and take the first direction
   keep taking directions until we reach ZZZ
*)

let get_next_direction d_idx d_len directions =
  let next = String.get directions !d_idx in
  d_idx := (!d_idx + 1) % d_len;
  next
;;

let build_map paths =
  List.fold paths ~init:Map.Poly.empty ~f:(fun acc path ->
    let non_letters_regex = Str.regexp "[^A-Z ]" in
    let letters_path = Str.global_replace non_letters_regex "" path in
    let path_parts =
      List.filter (String.split letters_path ~on:' ') ~f:(fun part ->
        String.length part > 0)
    in
    match path_parts with
    | key :: l :: r :: _ -> Map.add_exn acc ~key ~data:(l, r)
    | _ -> raise (Failure "coudldn't parse path"))
;;

let rec get_step_count map directions d_len from steps d_idx =
  match Map.Poly.find_exn map from with
  | l, r ->
    let next_from =
      match String.get directions d_idx with
      | 'R' ->
        (* Fmt.pr "@.%c" 'R'; *)
        r
      | 'L' ->
        (* Fmt.pr "@.%c" 'L'; *)
        l
      | _ -> raise (Failure "invalid next direction")
    in
    (match next_from with
     | "ZZZ" -> steps
     | _ ->
       get_step_count
         map
         directions
         d_len
         next_from
         (steps + 1)
         ((d_idx + 1) % d_len))
;;

let () =
  let directions = List.nth_exn lines 0 in
  let d_len = String.length directions in
  let d_idx = 0 in
  (* Remove the first line and any empty lines *)
  let paths =
    match lines with
    | _ :: tl -> List.filter tl ~f:(fun line -> String.length line > 0)
    | _ -> raise (Failure "couldn't parse paths")
  in
  let steps = get_step_count (build_map paths) directions d_len "AAA" 1 d_idx in
  Fmt.pr "@.%d" steps
;;

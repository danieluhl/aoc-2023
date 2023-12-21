open Core

let lines = Aoc_utils.read_file "inputs/day06.txt"
(* let lines = Aoc_utils.read_file "inputs/test.txt" *)

let get_nth l n =
  match List.nth l n with
  | None -> ""
  | Some x -> x
;;

let split_take_nth str nth del = get_nth (String.split str ~on:del) nth

let times =
  List.filter_map
    (String.split (split_take_nth (get_nth lines 0) 1 ':') ~on:' ')
    ~f:(fun d ->
      match d with
      | "" -> None
      | d -> Some (int_of_string d))
;;

let distances =
  List.filter_map
    (String.split (split_take_nth (get_nth lines 1) 1 ':') ~on:' ')
    ~f:(fun d ->
      match d with
      | "" -> None
      | d -> Some (int_of_string d))
;;

(*
   get the number of ways to win

   map over times
   store index so we can also look at the right time
   hold time is the number of seconds we're holiding it down
   time_wins - wins this time
   total_wins - each time win count multiplied together

   calculate the total time
   - total = (ms - time held) * (time held)

   if total > ms add to time_wins
   if ms >= time held
   - multiply total
   - clear time wins
   - go to next time
*)

let rec get_count_for_time time distance hold wins =
  if hold >= time
  then wins
  else (
    match (time - hold) * hold with
    | result when result > distance ->
      get_count_for_time time distance (hold + 1) (wins + 1)
    | _ -> get_count_for_time time distance (hold + 1) wins)
;;

let get_count_ways_to_win times distances =
  List.foldi times ~init:1 ~f:(fun index all_wins time ->
    let distance =
      match List.nth distances index with
      | None -> 1
      | Some x -> x
    in
    let count_for_next = get_count_for_time time distance 0 0 in
    Fmt.pr "@.%d" count_for_next;
    all_wins * count_for_next)
;;

let () = Fmt.pr "@.%d" (get_count_ways_to_win times distances)

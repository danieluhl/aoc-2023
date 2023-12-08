open Core

let lines = Read.read_file "inputs/day04.txt"
(* let lines = Read.read_file "inputs/test.txt" *)

let games =
  let game_right =
    List.filter_map lines ~f:(fun line ->
      List.nth (String.split line ~on:':') 1)
  in
  List.map game_right ~f:(fun game ->
    String.split game ~on:'|'
    |> List.map ~f:(fun nums ->
      String.split nums ~on:' '
      |> List.filter_map ~f:(fun num ->
        match num with
        | num when String.length (String.strip num) > 0 ->
          Some (int_of_string num)
        | _ -> None)))
;;

(*
   map over games checking if each item in one list is in the other list
*)

(* exp is the exponent we'll use to calculate the score, 2^exp *)
let get_win_count game =
  let mine =
    match List.nth game 0 with
    | Some l -> l
    | None -> raise (Failure "game format bad")
  in
  let yours =
    match List.nth game 1 with
    | Some l -> l
    | None -> raise (Failure "game format bad")
  in
  List.fold mine ~init:0 ~f:(fun acc num ->
    match List.exists yours ~f:(fun your_num -> your_num = num) with
    | true -> succ acc
    | _ -> acc)
;;

let get_score games =
  List.fold games ~init:0 ~f:(fun acc game ->
    match get_win_count game with
    | 0 -> acc
    | _ -> acc + Int.pow 2 (get_win_count game - 1))
;;

let () = Fmt.pr "%d\n" (get_score games)

(* List.map ~f:int_of_string)) *)

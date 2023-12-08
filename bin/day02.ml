let input_file = "inputs/day02.txt"

let extract_int str =
  let num_str = String.concat "" (Str.split (Str.regexp "[^0-9]") str) in
  match int_of_string_opt num_str with
  | Some num -> num
  | None -> raise (Failure "No valid integer found in the string")
;;

let extract_color str =
  try
    let _ =
      Str.search_forward (Str.regexp "\\b\\(red\\|green\\|blue\\)\\b") str 0
    in
    Some (Str.matched_string str)
  with
  | Not_found -> None
;;

let is_valid_draw draw =
  match extract_color draw with
  | Some x when x = "blue" -> extract_int draw < 15
  | Some x when x = "green" -> extract_int draw < 14
  | Some x when x = "red" -> extract_int draw < 13
  | _ -> false
;;

let is_valid_game_id line =
  let line_games = List.nth (String.split_on_char ':' line) 1 in
  let games_list = String.split_on_char ';' line_games in
  let draws_list =
    List.flatten (List.map (fun str -> String.split_on_char ',' str) games_list)
  in
  List.for_all is_valid_draw draws_list
;;

let get_game_id line =
  let game_str = List.nth (String.split_on_char ':' line) 0 in
  let game_num = extract_int game_str in
  match is_valid_game_id line with
  | true -> game_num
  | _ -> 0
;;

(* sum of the ids of the valid games *)
let rec read_lines file_input_channel lines =
  try
    let line = input_line file_input_channel in
    read_lines file_input_channel (line :: lines)
  with
  | End_of_file ->
    close_in file_input_channel;
    List.rev lines
  | _ ->
    close_in file_input_channel;
    raise (Failure "Error reading file")
;;

let () =
  let lines = read_lines (open_in input_file) [] in
  let sum = List.fold_left (fun acc line -> acc + get_game_id line) 0 lines in
  Printf.printf "%d\n" sum
;;

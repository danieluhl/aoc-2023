open Core

let lines = Read.read_file "inputs/day05.txt"
(* let lines = Read.read_file "inputs/test.txt" *)

let split_take_nth str nth del =
  match List.nth (String.split str ~on:del) nth with
  | Some x -> x
  | None -> ""
;;

let get_seeds line = String.split (split_take_nth line 1 ':') ~on:' '

(*
   Iterate through lines:
   read lines starting at index 1
   if empty, go next
   if character, we didn't find a matching range, iterate with the same location
   if number, parse the line and check if the number is in the range
   if yes, then shortcut out to the next line that has characters
   if no, go next
*)

let parse_map_line line =
  match String.split line ~on:' ' with
  | dest_start :: source_start :: range ->
    let source_start = Int.of_string source_start in
    let dest_start = Int.of_string dest_start in
    let range = Int.of_string (List.hd_exn range) in
    dest_start, source_start, range
  | _ -> 0, 0, 0
;;

let rec location_of_seed lines seed loc found =
  match lines with
  | [] -> loc
  | hd :: tl ->
    (match hd with
     | "" -> location_of_seed tl seed loc false
     | line when Char.is_digit line.[0] ->
       (match found with
        | true -> location_of_seed tl seed loc true
        | false ->
          let dest_start, source_start, range = parse_map_line line in
          Fmt.pr "@.%d-%d-%d : %d" dest_start source_start range loc;
          (match loc with
           | loc when loc >= source_start && loc <= source_start + range ->
             location_of_seed tl seed (loc + (dest_start - source_start)) true
           | _ -> location_of_seed tl seed loc false))
     | _ -> location_of_seed tl seed loc false)
;;

let get_lowest_location lines seeds =
  match lines with
  | [] -> 0
  | _ :: tail ->
    List.fold seeds ~init:Int.max_value ~f:(fun lowest seed ->
      Fmt.pr "@.-- %d -- " seed;
      match location_of_seed tail seed seed false with
      | loc when loc < lowest -> loc
      | _ -> lowest)
;;

let () =
  Fmt.pr
    "@.%d@."
    (get_lowest_location
       lines
       (List.filter_map
          (get_seeds (List.hd_exn lines))
          ~f:(fun seed ->
            match seed with
            | "" -> None
            | x -> Some (Int.of_string x))))
;;

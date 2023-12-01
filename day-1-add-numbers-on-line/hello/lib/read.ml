let rec concat_lines acc = 
  match acc with 
  | [] -> ""
  | h :: t -> h ^ "\n" ^ concat_lines t

let read_file filename = 
  let file = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line file in
      read_lines (line :: acc)
    with
    | End_of_file -> List.rev acc
  in
  let lines = read_lines [] in
  close_in file;
  lines






let is_char_int c = 
  let code = Char.code c in
  code >= Char.code '0' && code <= Char.code '9'

let first_last str = 
  let first_char = String.get str 0 in
  let last_char = String.get str (String.length str - 1) in 
  String.make 1 first_char ^ String.make 1 last_char

let int_from_line line = 
  String.to_seq line |> Seq.filter (fun c -> is_char_int c) |> String.of_seq |> first_last |> int_of_string

let str_lines_to_int =
  List.map (fun line -> int_from_line line)

let sum_from_int_lines = 
  List.fold_left (fun acc x -> acc + x) 0

let lines = Hello.Read.read_file "input.txt"

let () = Printf.printf "%d\n" (str_lines_to_int lines |> sum_from_int_lines)
      


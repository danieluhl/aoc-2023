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

type search_num = {
  num: int;
  str: string;
  dig: string;
}

let num_searches = [
  {num = 1; str = "one"; dig = "1"},
  {num = 2; str = "two"; dig = "2"},
  {num = 3; str = "three"; dig = "3"},
  {num = 4; str = "four"; dig = "4"},
  {num = 5; str = "five"; dig = "5"},
  {num = 6; str = "six"; dig = "6"},
  {num = 7; str = "seven"; dig = "7"},
  {num = 8; str = "eight"; dig = "8"},
  {num = 9; str = "nine"; dig = "9"}
]

let get_first_index str substr = 
  try
    let re = Str.

let int_from_line_2 line =
  let small = List.fold_left (fun acc x -> )



(*

get the index of each number in the list, both digit and string

store the largest and smallest as we go

*)      


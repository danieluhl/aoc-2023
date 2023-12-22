open Core

type search_num =
  { str : string
  ; ch : string
  }

let search_num_list =
  [ { str = "zero"; ch = "0" }
  ; { str = "one"; ch = "1" }
  ; { str = "two"; ch = "2" }
  ; { str = "three"; ch = "3" }
  ; { str = "four"; ch = "4" }
  ; { str = "five"; ch = "5" }
  ; { str = "six"; ch = "6" }
  ; { str = "seven"; ch = "7" }
  ; { str = "eight"; ch = "8" }
  ; { str = "nine"; ch = "9" }
  ]
;;

let find_substring_left str substr =
  try
    let regex = Str.regexp_string substr in
    let index = Str.search_forward regex str 0 in
    Some index
  with
  | _ -> None
;;

let find_substring_right str substr =
  try
    let regex = Str.regexp_string substr in
    let index = Str.search_backward regex str (String.length str) in
    Fmt.pr "%s %s %d\n" str substr index;
    Some index
  with
  | _ -> None
;;

type min_record =
  { ch : string
  ; i : int
  }

(* get index of first number *)
let get_left_number line =
  List.fold
    search_num_list
    ~init:{ ch = ""; i = Int.max_value }
    ~f:(fun acc x ->
      let left_ch_i =
        match find_substring_left line x.ch with
        | Some x -> x
        | _ -> Int.max_value
      in
      let left_str_i =
        match find_substring_left line x.str with
        | Some x -> x
        | _ -> Int.max_value
      in
      match acc with
      | { i; _ } when left_ch_i < i && left_ch_i < left_str_i ->
        { ch = x.ch; i = left_ch_i }
      | { i; _ } when left_str_i < i -> { ch = x.ch; i = left_str_i }
      | _ -> acc)
;;

let get_right_number line =
  List.fold search_num_list ~init:{ ch = "0"; i = -1 } ~f:(fun acc x ->
    let right_ch_i =
      match find_substring_right line x.ch with
      | Some x -> x
      | _ -> -1
    in
    let right_str_i =
      match find_substring_right line x.str with
      | Some x -> x
      | _ -> -1
    in
    match acc with
    | { i; _ } when right_ch_i > i && right_ch_i > right_str_i ->
      { ch = x.ch; i = right_ch_i }
    | { i; _ } when right_str_i > i -> { ch = x.ch; i = right_str_i }
    | _ -> acc)
;;

let () =
  let lines = Aoc_utils.read_file "inputs/day01.txt" in
  let result =
    List.fold
      ~init:0
      ~f:(fun acc line ->
        let { ch = left_ch; i = left_i } = get_left_number line in
        let { ch = right_ch; i = right_i } = get_right_number line in
        if left_i = right_i
        then acc + int_of_string left_ch
        else acc + int_of_string (left_ch ^ right_ch))
      lines
  in
  Printf.printf "\nresult: %d\n" result
;;

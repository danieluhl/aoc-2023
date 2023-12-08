open Core

(*
   create a matrix, for any x, y in the matrix
   get each direction from that point and see if any are symbols (not numbers or .)
   if it has a symbol, add to the count, otherwise, no problems

   iterate one at a time
   if it's a number, check all directions for a symbol and save that value
   keep grabbing numbers and checking until a non-number is found
   finally check if there were any symbols found and either add the number to
   the list or not
   continue on to the next character
   if it's not a number, ignore and move to next character
*)

let lines = Read.read_file "inputs/day03.txt"

let matrix =
  Array.of_list_map lines ~f:(fun line -> Array.of_list (String.to_list line))
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let adjacents = [ -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 ]

let is_symbol = function
  | '0' .. '9' -> false
  | '.' -> false
  | _ -> true
;;

let check_adjacents_for_symbols matrix x y =
  List.exists adjacents ~f:(fun (cx, cy) ->
    try is_symbol matrix.(y + cy).(x + cx) with
    | _ -> false)
;;

let rec get_sum matrix y x num sum has_symbol =
  let ch = matrix.(y).(x) in
  (* Printf.printf "y: %d, x: %d\n" y x; *)
  (* Printf.printf "num: %s\n" num; *)
  (* Printf.printf "sum: %d\n" sum; *)
  (* Printf.printf "has_symbol: %b\n" has_symbol; *)
  if is_digit ch
  then (
    let next_has_symbol =
      if not has_symbol then check_adjacents_for_symbols matrix x y else true
    in
    try
      (* try going to the next column *)
      get_sum matrix y (x + 1) (num ^ String.make 1 ch) sum next_has_symbol
    with
    | _ ->
      (try
         (* go to the next row *)
         let next_sum =
           if String.length num > 0 && next_has_symbol
           then sum + int_of_string (num ^ String.make 1 ch)
           else sum
         in
         get_sum matrix (y + 1) 0 "" next_sum false
       with
       | _ -> sum + int_of_string (num ^ String.make 1 ch)))
  else (
    let next_sum =
      if String.length num > 0 && has_symbol
      then sum + int_of_string num
      else sum
    in
    try
      (* try going to the next column *)
      get_sum matrix y (x + 1) "" next_sum false
    with
    | _ ->
      (try
         (* go to the next row *)
         get_sum matrix (y + 1) 0 "" next_sum false
       with
       | _ -> next_sum))
;;

let sum = get_sum matrix 0 0 "" 0 false
let () = Printf.printf "%d" sum

open Core
open Base.List.Assoc

let lines = Aoc_utils.read_file "inputs/day07.txt"
(* let lines = Aoc_utils.read_file "inputs/test.txt" *)

let get_nth l n =
  match List.nth l n with
  | None -> ""
  | Some x -> x
;;

let split_take_nth str nth del = get_nth (String.split str ~on:del) nth

(*
   sort the lines
   index is the multiplier, times the bid
   add them all up

   sort:
   get rank for each and compare
   if same, iterate through cards to find the winner
   if different, take the highest

   get card rank: check for each scenario in turn
   5 of a kind
   4 of a kind
   full house
   3 of a kind
   two pair
   pair
   none
     {number: count}
     - if 1 key: 5 of a kind
     - if 2 keys: check for 4 of a kind, full house
       - code 32 vs 41
     - if 3 keys: check for 3 of a kind or 2 pair
       - code 311 vs 221
     - if 4 keys: pair
     - if 5 keys: none


32T3K 765
T55J5 684
 {
    3: 2
    2: 1
    T: 1
    K: 1
    code: 2111 (length 4)
     }
 {
    code: 311 (length 3)
 }
  

  data structure for sorting:
     [
        {
           type rank: 0-6,
           cards: 12345,
      }
     ]
*)

let match_map = [ "14", 1; "23", 2; "113", 3; "122", 4 ]

let card_value_map =
  [ '2', 2
  ; '3', 3
  ; '4', 4
  ; '5', 5
  ; '6', 6
  ; '7', 7
  ; '8', 8
  ; '9', 9
  ; 'T', 10
  ; 'J', 11
  ; 'Q', 12
  ; 'K', 13
  ; 'A', 14
  ]
;;

let get_hand_code hand =
  let groups =
    String.to_list hand
    |> List.sort_and_group ~compare:(fun x y -> Char.compare x y)
  in
  let lengths =
    List.fold groups ~init:"" ~f:(fun acc group ->
      match group with
      | a when List.length a > 0 ->
        (* Fmt.pr "@.len: %s" (String.concat (List.map ~f:(String.make 1) a)); *)
        acc ^ string_of_int (List.length a)
      | _ -> acc)
  in
  let list_lens = String.to_list lengths in
  let sorted_lens =
    String.of_list (List.sort list_lens ~compare:Char.compare)
  in
  sorted_lens
;;

let rec compare_by_cards a b i =
  if i > 4
  then 0
  else (
    let a_next = String.get a i in
    let b_next = String.get b i in
    (* Fmt.pr "@.%c-%c" a_next b_next; *)
    match
      find_exn card_value_map ~equal:Char.equal a_next
      - find_exn card_value_map ~equal:Char.equal b_next
    with
    | 0 -> compare_by_cards a b (i + 1)
    | a -> a)
;;

let compare_lines a b =
  let a_hand = split_take_nth a 0 ' ' in
  let b_hand = split_take_nth b 0 ' ' in
  let a_code = get_hand_code a_hand in
  let b_code = get_hand_code b_hand in
  let b_len = String.length b_code in
  (* Fmt.pr "@.a: %s, b: %s" a_code b_code; *)
  match String.length a_code with
  | len when len < b_len -> 1
  | len when len > b_len -> -1
  | len when len = b_len ->
    (match
       ( find match_map ~equal:String.equal b_code
       , find match_map ~equal:String.equal a_code )
     with
     | Some a, Some b ->
       (match a - b with
        | 0 -> compare_by_cards a_hand b_hand 0
        | a -> a)
     | _ -> compare_by_cards a_hand b_hand 0)
  | _ -> 0
;;

let () =
  let sorted_lines = List.sort lines ~compare:(fun a b -> compare_lines a b) in
  let result =
    List.foldi sorted_lines ~init:0 ~f:(fun i acc line ->
      (* Fmt.pr "@.%s - %d" line (i + 1); *)
      let dstr = split_take_nth line 1 ' ' in
      let dig = int_of_string dstr in
      acc + (dig * (i + 1)))
  in
  Fmt.pr "@.%d" result
;;

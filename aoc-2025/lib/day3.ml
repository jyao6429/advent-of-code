open! Core

let rec try_combine front rest ~acc =
  match rest with
  | [] -> acc
  | x :: xs ->
    let combined = (front * 10) + x in
    let acc = max acc combined in
    try_combine front xs ~acc
;;

let rec max_joltage' bank ~acc =
  match bank with
  | [] -> acc
  | x :: xs ->
    let acc = try_combine x xs ~acc in
    max_joltage' xs ~acc
;;

let max_joltage bank = max_joltage' bank ~acc:0

let puzzle_1 input =
  let banks =
    List.map input ~f:(fun line -> String.to_list line |> List.map ~f:Char.get_digit_exn)
  in
  List.sum (module Int) banks ~f:max_joltage
;;

let%expect_test "puzzle 1 - example" =
  let input = {| **REMOVED** |} in
  let lines = String.split_lines (String.strip input) in
  puzzle_1 lines |> Common.print_int;
  [%expect {| 357 |}]
;;

let%expect_test "puzzle 1 - input" =
  let input = Common.read_lines "day3.txt" in
  puzzle_1 input |> Common.print_int;
  [%expect {| 16993 |}]
;;

let num_to_turn_on = 12

let rec max_joltage_multi' bank memo ~num_turned_on ~idx ~acc =
  if num_turned_on = num_to_turn_on
  then List.rev acc |> List.fold ~init:0 ~f:(fun acc d -> (acc * 10) + d)
  else if idx >= Array.length bank
  then 0
  else if memo.(num_turned_on).(idx) <> 0
  then memo.(num_turned_on).(idx)
  else (
    let curr = bank.(idx) in
    let with_curr =
      max_joltage_multi'
        bank
        memo
        ~num_turned_on:(num_turned_on + 1)
        ~idx:(idx + 1)
        ~acc:(curr :: acc)
    in
    let without_curr = max_joltage_multi' bank memo ~num_turned_on ~idx:(idx + 1) ~acc in
    let best = max with_curr without_curr in
    memo.(num_turned_on).(idx) <- best;
    best)
;;

let max_joltage_multi bank =
  let bank = List.to_array bank in
  let len = Array.length bank in
  let memo = Array.init num_to_turn_on ~f:(fun _ -> Array.create ~len 0) in
  let best = max_joltage_multi' bank memo ~num_turned_on:0 ~idx:0 ~acc:[] in
  memo |> [%sexp_of: int array array] |> print_s;
  Common.print_int best;
  best
;;

let puzzle_2 input =
  let banks =
    List.map input ~f:(fun line -> String.to_list line |> List.map ~f:Char.get_digit_exn)
  in
  List.sum (module Int) banks ~f:max_joltage_multi
;;

let%expect_test "puzzle 2 - example" =
  let input = {| **REMOVED** |} in
  let lines = String.split_lines (String.strip input) in
  puzzle_2 lines |> Common.print_int;
  [%expect {| 2065786182157 |}]
;;

(* let%expect_test "puzzle 2 - input" =
  let input = Common.read_lines "day3.txt" in
  puzzle_2 input |> Common.print_int;
  [%expect {| 16993 |}]
;; *)

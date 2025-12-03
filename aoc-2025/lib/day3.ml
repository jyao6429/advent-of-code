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
let puzzle_2 input =
  let banks =
    List.map input ~f:(fun line -> String.to_list line |> List.map ~f:Char.get_digit_exn)
  in
  List.sum (module Int) banks ~f:max_joltage
;;

let%expect_test "puzzle 2 - example" =
  let input = {| **REMOVED** |} in
  let lines = String.split_lines (String.strip input) in
  puzzle_2 lines |> Common.print_int;
  [%expect {| 357 |}]
;;

let%expect_test "puzzle 2 - input" =
  let input = Common.read_lines "day3.txt" in
  puzzle_2 input |> Common.print_int;
  [%expect {| 16993 |}]
;;

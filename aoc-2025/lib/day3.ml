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
let digit_order = List.init 10 ~f:Fn.id |> List.rev

let find_next_digit bank_len index_map ~idx ~digit =
  List.find_map_exn digit_order ~f:(fun d ->
    let%bind.Option indices = Map.find index_map d in
    let%bind.Option next_idx =
      Set.binary_search indices ~compare `First_greater_than_or_equal_to idx
    in
    if bank_len - (num_to_turn_on - digit) < next_idx
    then None
    else Some (~next_digit:d, ~next_idx:(next_idx + 1)))
;;

let max_joltage_multi bank =
  let bank_len = List.length bank in
  let index_map =
    List.foldi
      bank
      ~init:(Map.empty (module Int))
      ~f:(fun i acc d ->
        Map.update acc d ~f:(function
          | None -> Set.singleton (module Int) i
          | Some indices -> Set.add indices i))
  in
  let ~number, ~idx:_ =
    List.init num_to_turn_on ~f:Fn.id
    |> List.fold ~init:(~number:0, ~idx:0) ~f:(fun (~number, ~idx) digit ->
      let ~next_digit, ~next_idx = find_next_digit bank_len index_map ~idx ~digit in
      let number = (number * 10) + next_digit in
      ~number, ~idx:next_idx)
  in
  number
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
  [%expect {| 3121910778619 |}]
;;

let%expect_test "puzzle 2 - input" =
  let input = Common.read_lines "day3.txt" in
  puzzle_2 input |> Common.print_int;
  [%expect {| 168617068915447 |}]
;;

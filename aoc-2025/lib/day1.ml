open! Core

let modulo = 100
let initial_pos = 50

let get_operations lines =
  List.map lines ~f:(fun line ->
    let number = String.drop_prefix line 1 |> Int.of_string in
    match String.prefix line 1 with
    | "L" -> -number
    | "R" -> number
    | _ -> failwith "invalid input")
;;

let puzzle_1 lines =
  let operations = get_operations lines in
  let ~pos:_, ~count =
    List.fold operations ~init:(~pos:initial_pos, ~count:0) ~f:(fun (~pos, ~count) op ->
      let pos = (pos + op + modulo) mod modulo in
      let count = if pos = 0 then count + 1 else count in
      ~pos, ~count)
  in
  count
;;

let%expect_test "puzzle 1 - example" =
  let lines = {| **REMOVED** |} in
  let lines = String.split_lines (String.strip lines) in
  let count = puzzle_1 lines in
  Printf.printf "%d\n" count;
  [%expect {| 3 |}]
;;

(* let%expect_test "puzzle 1 - input" =
  let lines =
    In_channel.read_lines
      "/home/jyao6429/oxcaml/advent-of-code/aoc-2025/resources/day1.txt"
  in
  let count = puzzle_1 lines in
  Printf.printf "%d\n" count;
  [%expect {| 1089 |}]
;; *)

let puzzle_2 lines =
  let operations = get_operations lines in
  let ~pos:_, ~count =
    List.fold operations ~init:(~pos:initial_pos, ~count:0) ~f:(fun (~pos, ~count) op ->
      let full_rotations = Int.abs op / modulo in
      let op = op mod modulo in
      let new_unmodded_pos = pos + op in
      let should_add_additional =
        (pos > 0 && new_unmodded_pos <= 0) || new_unmodded_pos >= modulo
      in
      let pos = (new_unmodded_pos + modulo) mod modulo in
      let count = count + full_rotations + Bool.to_int should_add_additional in
      (* print_s
        [%message
          (op : int)
            (new_unmodded_pos : int)
            (pos : int)
            (should_add_additional : bool)
            (full_rotations : int)
            (count : int)]; *)
      ~pos, ~count)
  in
  count
;;

let%expect_test "puzzle 2 - example" =
  let lines = {| **REMOVED** |} in
  let lines = String.split_lines (String.strip lines) in
  let count = puzzle_2 lines in
  Printf.printf "%d\n" count;
  [%expect
    {| 6 |}]
;;

let%expect_test "puzzle 2 - input" =
  let lines =
    In_channel.read_lines
      "/home/jyao6429/oxcaml/advent-of-code/aoc-2025/resources/day1.txt"
  in
  let count = puzzle_2 lines in
  Printf.printf "%d\n" count;
  [%expect {| 6530 |}]
;;

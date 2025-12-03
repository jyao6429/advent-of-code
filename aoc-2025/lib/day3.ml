open! Core

let puzzle_1 input = failwith "heh"

let%expect_test "puzzle 1 - example" =
  let input = "blergh" in
  puzzle_1 input |> Common.print_int;
  [%expect {| 0 |}]
;;

(* let%expect_test "puzzle 1 - input" =
  let input = Common.read_all "day3.txt" in
  puzzle_1 input |> Common.print_int;
  [%expect {| 0 |}]
;; *)

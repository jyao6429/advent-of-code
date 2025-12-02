open! Core

let run _lines = 0

let%expect_test "example" =
  let lines = {| **REMOVED** |} in
  let count = run lines in
  Printf.printf "%d\n" count;
  [%expect {| Running Day 1 solution |}]
;;

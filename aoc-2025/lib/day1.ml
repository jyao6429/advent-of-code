open! Core

let run () = print_endline "Running Day 1 solution"


let%expect_test "day1" =
  run ();
  [%expect {| Running Day 1 solution |}]
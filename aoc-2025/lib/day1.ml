open! Core

let modulo = 100
let initial_pos = 50

let run lines =
  let operations =
    List.map lines ~f:(fun line ->
      let number = String.drop_prefix line 1 |> Int.of_string in
      match String.prefix line 1 with
      | "L" -> -number
      | "R" -> number
      | _ -> failwith "invalid input")
  in
  let ~pos:_, ~count =
    List.fold operations ~init:(~pos:initial_pos, ~count:0) ~f:(fun (~pos, ~count) op ->
      let pos = (pos + op + modulo) mod modulo in
      let count = if pos = 0 then count + 1 else count in
      ~pos, ~count)
  in
  count
;;

let%expect_test "example" =
  let lines = {| **REMOVED** |} in
  let lines = String.split_lines (String.strip lines) in
  let count = run lines in
  Printf.printf "%d\n" count;
  [%expect {| 3 |}]
;;

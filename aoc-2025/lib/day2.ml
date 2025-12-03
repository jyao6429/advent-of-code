open! Core

let puzzle_1 input =
  let ranges =
    String.split input ~on:','
    |> List.map ~f:(fun range ->
      match String.split range ~on:'-' with
      | [ start_str; end_str ] -> Int.of_string start_str, Int.of_string end_str
      | _ -> failwith "Invalid range format")
  in
  let sum = List.fold ranges ~init:0 ~f:(fun sum (first, last)-> 
    let sum_ref = ref 0 in
    for i = first to last do
      let str = Int.to_string i in
      let len = String.length str in
      if len % 2 = 0 then (
        let half_str = String.subo str ~len:(len / 2) in
        if String.equal str (half_str ^ half_str) then
          sum_ref := !sum_ref + i
      )
    done;
    sum + !sum_ref
    ) in
  sum
;;

let%expect_test "puzzle 1 - example" =
  let input = {| **REMOVED** |}
  in
  puzzle_1 input |> Printf.printf "%d\n" ;
  [%expect {| 1227775554 |}]
;;

let%expect_test "puzzle 1 - input" =
  let input =
     In_channel.read_all
      "/home/jyao6429/oxcaml/advent-of-code/aoc-2025/resources/day2.txt"
  in
  puzzle_1 input |> Printf.printf "%d\n" ;
  [%expect {| 29940924880 |}]
;;

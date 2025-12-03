open! Core

let puzzle_1 input =
  let ranges =
    String.split input ~on:','
    |> List.map ~f:(fun range ->
      match String.split range ~on:'-' with
      | [ start_str; end_str ] -> Int.of_string start_str, Int.of_string end_str
      | _ -> failwith "Invalid range format")
  in
  let count = List.fold ranges ~init:0 ~f:(fun count (first, last)-> 
    let counter = ref 0 in
    for i = first to last do
      let str = Int.to_string i in
      let len = String.length str in
      if len % 2 = 0 then (
        let half_str = String.subo str ~len:(len / 2) in
        if String.equal str (half_str ^ half_str) then
          incr counter
      )
    done;
    count + !counter
    ) in
  count
;;

let%expect_test "puzzle 1 - example" =
  let input = {| **REMOVED** |}
  in
  puzzle_1 input |> Printf.printf "%d\n" ;
  [%expect {| 8 |}]
;;

open! Core

let puzzle_1 input =
  let ranges =
    String.split input ~on:','
    |> List.map ~f:(fun range ->
      match String.split range ~on:'-' with
      | [ start_str; end_str ] -> Int.of_string start_str, Int.of_string end_str
      | _ -> failwith "Invalid range format")
  in
  let sum =
    List.fold ranges ~init:0 ~f:(fun sum (first, last) ->
      let sum_ref = ref 0 in
      for i = first to last do
        let str = Int.to_string i in
        let len = String.length str in
        if len % 2 = 0
        then (
          let half_str = String.subo str ~len:(len / 2) in
          if String.equal str (half_str ^ half_str) then sum_ref := !sum_ref + i)
      done;
      sum + !sum_ref)
  in
  sum
;;

let%expect_test "puzzle 1 - example" =
  let input = Common.read_all "day2_example.txt" in
  puzzle_1 input |> Common.print_int;
  [%expect {| 1227775554 |}]
;;

let%expect_test "puzzle 1 - input" =
  let input = Common.read_all "day2.txt" in
  puzzle_1 input |> Common.print_int;
  [%expect {| 29940924880 |}]
;;

let check_invalid n =
  let n_str = Int.to_string n in
  let n_len = String.length n_str in
  let repeat_lens_to_check =
    List.range ~stop:`inclusive 1 (n_len / 2) |> List.filter ~f:(fun l -> n_len % l = 0)
  in
  List.exists repeat_lens_to_check ~f:(fun l ->
    let to_repeat = String.subo n_str ~len:l in
    let repeated = String.init n_len ~f:(fun i -> String.get to_repeat (i mod l)) in
    String.equal n_str repeated)
;;

let puzzle_2 input =
  let ranges =
    String.split input ~on:','
    |> List.map ~f:(fun range ->
      match String.split range ~on:'-' with
      | [ start_str; end_str ] -> Int.of_string start_str, Int.of_string end_str
      | _ -> failwith "Invalid range format")
  in
  let to_check =
    List.concat_map ranges ~f:(fun (first, last) ->
      List.range ~stop:`inclusive first last)
  in
  let invalid = List.filter to_check ~f:check_invalid in
  List.sum (module Int) invalid ~f:Fn.id
;;

let%expect_test "puzzle 2 - example" =
  let input = Common.read_all "day2_example.txt" in
  puzzle_2 input |> Common.print_int;
  [%expect {| 4174379265 |}]
;;

let%expect_test "puzzle 2 - input" =
  let input = Common.read_all "day2.txt" in
  puzzle_2 input |> Common.print_int;
  [%expect {| 48631958998 |}]
;;

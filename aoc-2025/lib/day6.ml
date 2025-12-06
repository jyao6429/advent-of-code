open! Core

let day = "day6"

let example_input = Common.read_lines (day ^ "_example.txt")
;;

let prod_input = Common.read_lines (day ^ ".txt")

let process_input input =
  let problems_transposed =
    List.map input ~f:(fun line ->
      String.split ~on:' ' line |> List.filter ~f:(fun str -> String.is_empty str |> not))
  in
  let numbers, operations =
    List.split_n problems_transposed (List.length problems_transposed - 1)
  in
  let operations = List.nth_exn operations 0 in
  let numbers = List.map numbers ~f:(List.map ~f:Int.of_string) |> List.transpose_exn in
  ~numbers, ~operations
;;

let example_input = example_input |> process_input
let prod_input = prod_input |> process_input

module Part_1 = struct
  let solve (~numbers, ~operations) =
    let zipped = List.zip_exn numbers operations in
    List.sum (module Int) zipped ~f:(fun (numbers, char) ->
      let op, identity =
        match char with
        | "*" -> ( * ), 1
        | "+" -> ( + ), 0
        | _ -> failwith "invalid"
      in
      List.fold numbers ~init:identity ~f:op)
  ;;

  module%test [@name "part_1"] _ = struct
    let%expect_test "example" =
      solve example_input |> Common.print_int;
      [%expect {| 4277556 |}]
    ;;

    let%expect_test "prod" =
      solve prod_input |> Common.print_int;
      [%expect {| 0 |}]
    ;;
  end
end

module Part_2 = struct
  let solve input =
    ignore input;
    0
  ;;

  module%test [@name "part_2"] _ = struct
    let%expect_test "example" =
      solve example_input |> Common.print_int;
      [%expect {| 0 |}]
    ;;

    let%expect_test "prod" =
      solve prod_input |> Common.print_int;
      [%expect {| 0 |}]
    ;;
  end
end

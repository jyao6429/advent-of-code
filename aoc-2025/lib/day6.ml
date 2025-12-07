open! Core

let day = "day6"
let example_input = Common.read_lines (day ^ "_example.txt")
let prod_input = Common.read_lines (day ^ ".txt")

module Part_1 = struct
  let process_input input =
    let problems_transposed =
      List.map input ~f:(fun line ->
        String.split ~on:' ' line
        |> List.filter ~f:(fun str -> String.is_empty str |> not))
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
      [%expect {| 4648618073226 |}]
    ;;
  end
end

module Part_2 = struct
  let process_input input =
    let problems_transposed = List.map input ~f:String.to_list in
    let numbers, operations =
      List.split_n problems_transposed (List.length problems_transposed - 1)
    in
    let operations = List.nth_exn operations 0 in
    let numbers = numbers |> List.transpose_exn in
    ~numbers, ~operations
  ;;

  let example_input = example_input |> process_input
  let prod_input = prod_input |> process_input

  let solve (~numbers, ~operations) =
    let operations = List.filter operations ~f:(fun c -> Char.is_whitespace c |> not) in
    let numbers =
      List.map numbers ~f:(fun column -> String.of_char_list column |> String.strip)
    in
    let grouped =
      List.fold numbers ~init:[ [] ] ~f:(fun acc num ->
        match num with
        | "" -> [] :: acc
        | num ->
          let num = Int.of_string num in
          (match acc with
           | [] -> failwith "impossible"
           | x :: xs -> (num :: x) :: xs))
      |> List.rev
    in
    let zipped = List.zip_exn grouped operations in
    List.sum (module Int) zipped ~f:(fun (numbers, char) ->
      let op, identity =
        match char with
        | '*' -> ( * ), 1
        | '+' -> ( + ), 0
        | _ -> failwith "invalid"
      in
      List.fold numbers ~init:identity ~f:op)
  ;;

  module%test [@name "part_2"] _ = struct
    let%expect_test "example" =
      solve example_input |> Common.print_int;
      [%expect {| 3263827 |}]
    ;;

    let%expect_test "prod" =
      solve prod_input |> Common.print_int;
      [%expect {| 7329921182115 |}]
    ;;
  end
end

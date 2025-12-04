open! Core

let day = "day4"

let example_input = Common.read_lines (day ^ "_example.txt")
;;

let prod_input = Common.read_lines (day ^ ".txt")

module Cell = struct
  type t =
    | Empty
    | Roll

  let t_of_char = function
    | '.' -> Empty
    | '@' -> Roll
    | c -> failwithf "Invalid cell character: %c" c ()
  ;;

  let char_of_t = function
    | Empty -> '.'
    | Roll -> '@'
  ;;
end

let process_input input =
  List.to_array input
  |> Array.map ~f:(fun line -> String.to_array line |> Array.map ~f:Cell.t_of_char)
;;

let example_input = example_input |> process_input
let prod_input = prod_input |> process_input

module Part_1 = struct
  let offsets = [ -1; 0; 1 ]

  let x_y_offsets =
    List.cartesian_product offsets offsets
    |> List.filter ~f:(fun (dx, dy) -> not (dx = 0 && dy = 0))
  ;;

  let max_adjacent = 3

  let check_adjacent grid cell ~i ~j =
    match cell with
    | Cell.Empty -> false
    | Roll ->
      let adjacent_rolls =
        List.count x_y_offsets ~f:(fun (dx, dy) ->
          let i = i + dx in
          let j = j + dy in
          if i < 0 || j < 0 || i >= Array.length grid || j >= Array.length grid.(0)
          then false
          else (
            match grid.(i).(j) with
            | Cell.Empty -> false
            | Roll -> true))
      in
      adjacent_rolls <= max_adjacent
  ;;

  let solve grid =
    let accessible =
      Array.mapi grid ~f:(fun i row ->
        Array.counti row ~f:(fun j cell -> check_adjacent grid cell ~i ~j))
    in
    Array.sum (module Int) accessible ~f:Fn.id
  ;;

  module%test [@name "part_1"] _ = struct
    let%expect_test "example" =
      solve example_input |> Common.print_int;
      [%expect {| 13 |}]
    ;;

    let%expect_test "prod" =
      solve prod_input |> Common.print_int;
      [%expect {| 1464 |}]
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

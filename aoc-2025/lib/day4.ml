open! Core

let day = "day4"

let example_input = Common.read_lines (day ^ "_example.txt")
;;

let prod_input = Common.read_lines (day ^ ".txt")

module Cell = struct
  type t : value mod portable =
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
  input
  |> Iarray.of_list_map ~f:(fun line ->
    String.to_sequence line |> Iarray.of_sequence |> Iarray.map ~f:Cell.t_of_char)
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
          if i < 0 || j < 0 || i >= Iarray.length grid || j >= Iarray.length grid.:(0)
          then false
          else (
            match grid.:(i).:(j) with
            | Cell.Empty -> false
            | Roll -> true))
      in
      adjacent_rolls <= max_adjacent
  ;;

  let solve grid =
    let accessible =
      Iarray.mapi grid ~f:(fun i row ->
        Iarray.counti row ~f:(fun j cell -> check_adjacent grid cell ~i ~j))
    in
    Iarray.sum (module Int) accessible ~f:Fn.id
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
  module Parray = Parallel_arrays.Iarray

  let offsets = [ -1; 0; 1 ]

  let x_y_offsets =
    List.cartesian_product offsets offsets
    |> List.filter ~f:(fun (dx, dy) -> not (dx = 0 && dy = 0))
  ;;

  let max_adjacent = 3

  let check_adjacent (grid : Cell.t Iarray.t Parray.t) cell ~i ~j =
    match cell with
    | Cell.Empty -> false
    | Roll ->
      let adjacent_rolls =
        List.count x_y_offsets ~f:(fun (dx, dy) ->
          let i = i + dx in
          let j = j + dy in
          if i < 0
             || j < 0
             || i >= Parray.length grid
             || j >= Iarray.length (Parray.get grid 0)
          then false
          else (
            match (Parray.get grid i).:(j) with
            | Cell.Empty -> false
            | Roll -> true))
      in
      adjacent_rolls <= max_adjacent
  ;;

  let solve (par @ local) (grid : Cell.t iarray iarray) =
    let grid = Parray.of_iarray grid in
    let step (grid : Cell.t iarray Parray.t) =
      let still_accessible =
        Parray.mapi par grid ~f:(fun i row ->
          Iarray.fold_mapi ~init:0 row ~f:(fun j acc cell ->
            if check_adjacent grid cell ~i ~j then acc + 1, Cell.Empty else acc, cell))
      in
      let new_grid = Parray.map par still_accessible ~f:snd in
      let num_accessible =
        Parray.map par still_accessible ~f:fst
        |> Parray.fold par ~init:(fun () -> 0) ~f:( + ) ~combine:( + )
      in
      ~new_grid, ~num_accessible
    in
    let rec loop (grid : Cell.t iarray Parray.t) ~total_accessible =
      let ~new_grid, ~num_accessible = step grid in
      let total_accessible = total_accessible + num_accessible in
      if num_accessible = 0 then total_accessible else loop new_grid ~total_accessible
    in
    loop grid ~total_accessible:0 [@nontail]
  ;;

  module%test [@name "part_2"] _ = struct
    let%expect_test "example" =
      Common.run_in_parallel ~f:(fun par -> solve par example_input) |> Common.print_int;
      [%expect {| 43 |}]
    ;;

    let%expect_test "prod - parallel" =
      let start_time = Time_ns.now () in
      let res = Common.run_in_parallel ~f:(fun par -> solve par prod_input) in
      let end_time = Time_ns.now () in
      let _duration = Time_ns.diff end_time start_time in
      print_s [%message (res : int)];
      [%expect {| (res 8409) |}]
    ;;

    let%expect_test "prod - sequential" =
      let start_time = Time_ns.now () in
      let res = Common.run_sequentially ~f:(fun par -> solve par prod_input) in
      let end_time = Time_ns.now () in
      let _duration = Time_ns.diff end_time start_time in
      print_s [%message (res : int)];
      [%expect {| (res 8409) |}]
    ;;
  end
end

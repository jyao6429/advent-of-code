open! Core

let day = "day7"
let example_input = Common.read_lines (day ^ "_example.txt")
let prod_input = Common.read_lines (day ^ ".txt")

module Cell = struct
  type t =
    | Empty
    | Start
    | Splitter
  [@@deriving equal]

  let char_of_t = function
    | Empty -> '.'
    | Start -> 'S'
    | Splitter -> '^'
  ;;

  let sexp_of_t t = t |> char_of_t |> sexp_of_char

  let t_of_char = function
    | '.' -> Empty
    | 'S' -> Start
    | '^' -> Splitter
    | _ -> failwith "invalid"
  ;;
end

let process_input input =
  let grid =
    List.map input ~f:(fun line ->
      String.to_sequence line |> Iarray.of_sequence |> Iarray.map ~f:Cell.t_of_char)
  in
  let start, grid = List.split_n grid 1 in
  let start = List.hd_exn start in
  let start_idx =
    Iarray.findi start ~f:(fun _ cell -> Cell.equal cell Cell.Start)
    |> Option.value_exn
    |> fst
  in
  ~start_idx, ~grid
;;

let example_input = example_input |> process_input
let prod_input = prod_input |> process_input

module Part_1 = struct
  let solve (~start_idx, ~grid) =
    let ~beams:_, ~num_splits =
      List.fold
        grid
        ~init:(~beams:(Set.singleton (module Int) start_idx), ~num_splits:0)
        ~f:(fun (~beams, ~num_splits) line ->
          let split, remaining =
            Set.partition_tf beams ~f:(fun idx -> Cell.equal line.:(idx) Cell.Splitter)
          in
          let new_beams =
            Set.fold split ~init:remaining ~f:(fun acc idx ->
              let acc = Set.add acc (idx - 1) in
              let acc = Set.add acc (idx + 1) in
              acc)
          in
          ~beams:new_beams, ~num_splits:(num_splits + Set.length split))
    in
    num_splits
  ;;

  module%test [@name "part_1"] _ = struct
    let%expect_test "example" =
      solve example_input |> Common.print_int;
      [%expect {| 21 |}]
    ;;

    let%expect_test "prod" =
      solve prod_input |> Common.print_int;
      [%expect {| 1630 |}]
    ;;
  end
end

module Part_2 = struct
  let solve (~start_idx, ~grid) =
    let timelines =
      List.fold
        grid
        ~init:(Map.singleton (module Int) start_idx 1)
        ~f:(fun timelines line ->
          let split, remaining =
            Map.partitioni_tf timelines ~f:(fun ~key:idx ~data:_ ->
              Cell.equal line.:(idx) Cell.Splitter)
          in
          let new_timelines =
            Map.fold split ~init:remaining ~f:(fun ~key:idx ~data:count acc ->
              let acc =
                Map.update acc (idx - 1) ~f:(fun value ->
                  Option.value value ~default:0 |> ( + ) count)
              in
              let acc =
                Map.update acc (idx + 1) ~f:(fun value ->
                  Option.value value ~default:0 |> ( + ) count)
              in
              acc)
          in
          new_timelines)
    in
    Map.sum (module Int) timelines ~f:Fn.id
  ;;

  module%test [@name "part_2"] _ = struct
    let%expect_test "example" =
      solve example_input |> Common.print_int;
      [%expect {| 40 |}]
    ;;

    let%expect_test "prod" =
      solve prod_input |> Common.print_int;
      [%expect {| 47857642990160 |}]
    ;;
  end
end

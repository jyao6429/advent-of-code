open! Core

let day = "day5"

let example_input = Common.read_lines (day ^ "_example.txt")

let prod_input = Common.read_lines (day ^ ".txt")

let process_input input =
  let fresh_ranges, ingredients =
    List.split_while input ~f:(fun line -> not (String.is_empty line))
  in
  let ingredients = List.tl_exn ingredients |> List.map ~f:Int.of_string in
  let fresh_increments =
    List.concat_map fresh_ranges ~f:(fun range ->
      let range = String.split range ~on:'-' |> List.map ~f:Int.of_string in
      match range with
      | [ a; b ] -> [ a, 1; b + 1, -1 ]
      | _ -> failwith "Invalid range")
  in
  let fresh_map =
    List.Assoc.sort_and_group ~compare fresh_increments
    |> List.map ~f:(fun (pos, deltas) -> pos, List.sum (module Int) deltas ~f:Fn.id)
    |> List.folding_map ~init:0 ~f:(fun acc (pos, delta) ->
      let acc = acc + delta in
      acc, (pos, acc))
    |> Map.of_alist_exn (module Int)
  in
  ~fresh_map, ~ingredients
;;

let example_input = example_input |> process_input
let prod_input = prod_input |> process_input

module Part_1 = struct
  let solve (~fresh_map, ~ingredients) =
    List.count ingredients ~f:(fun ingredient ->
      match Map.closest_key fresh_map `Less_or_equal_to ingredient with
      | None -> false
      | Some (_, 0) -> false
      | Some (_, count) ->
        assert (count > 0);
        true)
  ;;

  module%test [@name "part_1"] _ = struct
    let%expect_test "example" =
      solve example_input |> Common.print_int;
      [%expect {| 3 |}]
    ;;

    let%expect_test "prod" =
      solve prod_input |> Common.print_int;
      [%expect {| 775 |}]
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

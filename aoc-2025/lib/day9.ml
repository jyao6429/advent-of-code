open! Core

let day = "day9"
let example_input = Common.read_lines (day ^ "_example.txt")
let prod_input = Common.read_lines (day ^ ".txt")

let process_input input =
  List.map input ~f:(fun line ->
    match String.split line ~on:',' with
    | [ i; j ] -> Int.of_string i, Int.of_string j
    | _ -> failwith "invalid")
;;

let example_input = example_input |> process_input
let prod_input = prod_input |> process_input

let area (i1, j1) (i2, j2) =
  let i_lo, i_hi = Int.min i1 i2, Int.max i1 i2 in
  let j_lo, j_hi = Int.min j1 j2, Int.max j1 j2 in
  let area = (i_hi - i_lo + 1) * (j_hi - j_lo + 1) in
  (* print_s [%message "area" ((i1, j1) : int * int) ((i2, j2) : int * int) (area : int)]; *)
  area
;;

module Part_1 = struct
  let solve input =
    List.cartesian_product input input
    |> List.map ~f:(fun (p1, p2) -> area p1 p2)
    |> List.max_elt ~compare
    |> Option.value_exn
  ;;

  module%test [@name "part_1"] _ = struct
    let%expect_test "example" =
      solve example_input |> Common.print_int;
      [%expect {| 50 |}]
    ;;

    let%expect_test "prod" =
      solve prod_input |> Common.print_int;
      [%expect {| 4756718172 |}]
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

    (* let%expect_test "prod" =
      solve prod_input |> Common.print_int;
      [%expect {| 0 |}]
    ;; *)
  end
end

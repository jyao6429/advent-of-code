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

module Part_1 = struct
  let area (i1, j1) (i2, j2) =
    let i_lo, i_hi = Int.min i1 i2, Int.max i1 i2 in
    let j_lo, j_hi = Int.min j1 j2, Int.max j1 j2 in
    let area = (i_hi - i_lo + 1) * (j_hi - j_lo + 1) in
    (* print_s [%message "area" ((i1, j1) : int * int) ((i2, j2) : int * int) (area : int)]; *)
    area
  ;;

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
  let area (i1, j1) (i2, j2) =
    let i_lo, i_hi = Int.min i1 i2, Int.max i1 i2 in
    let j_lo, j_hi = Int.min j1 j2, Int.max j1 j2 in
    let area = (i_hi - i_lo + 1) * (j_hi - j_lo + 1) in
    (* print_s [%message "area" ((i1, j1) : int * int) ((i2, j2) : int * int) (area : int)]; *)
    area, ~tl:(i_lo, j_lo), ~br:(i_hi, j_hi)
  ;;

  let point_in_shape red_tiles (i, j) (i_restriction, j_restriction) =
    let i_to_js_to_search =
      match i_restriction with
      | `Lower -> Map.subrange red_tiles ~lower_bound:Unbounded ~upper_bound:(Incl i)
      | `Upper -> Map.subrange red_tiles ~lower_bound:(Incl i) ~upper_bound:Unbounded
    in
    let js_to_search = Map.data i_to_js_to_search |> Set.union_list (module Int) in
    let j_found =
      match j_restriction with
      | `Lower -> Set.binary_search js_to_search ~compare `Last_less_than_or_equal_to j
      | `Upper ->
        Set.binary_search js_to_search ~compare `First_greater_than_or_equal_to j
    in
    Option.is_some j_found
  ;;

  let is_valid red_tiles ~tl:(i_lo, j_lo) ~br:(i_hi, j_hi) =
    let point_in_shape = point_in_shape red_tiles in
    let could_be_in_shape =
      point_in_shape (i_lo, j_lo) (`Lower, `Lower)
      && point_in_shape (i_lo, j_hi) (`Lower, `Upper)
      && point_in_shape (i_hi, j_lo) (`Upper, `Lower)
      && point_in_shape (i_hi, j_hi) (`Upper, `Upper)
    in
    match could_be_in_shape with
    | false -> false
    | true ->
      let restricted_i =
        Map.subrange red_tiles ~lower_bound:(Excl i_lo) ~upper_bound:(Excl i_hi)
      in
      let restricted_i_unrestricted_j =
        Map.data restricted_i |> Set.union_list (module Int)
      in
      let j_hi_to_check =
        Set.binary_search
          restricted_i_unrestricted_j
          ~compare
          `First_strictly_greater_than
          j_lo
      in
      (* print_s
        [%message
          "is_valid"
            ((i_lo, j_lo) : int * int)
            ((i_hi, j_hi) : int * int)
            (restricted_i : Int.Set.t Int.Map.t)
            (restricted_i_unrestricted_j : Int.Set.t)
            (j_hi_to_check : int option)]; *)
      (match j_hi_to_check with
       | None -> true
       | Some j_hi_to_check -> j_hi <= j_hi_to_check)
  ;;

  let solve input =
    let red_tiles =
      Map.of_alist_multi (module Int) input |> Map.map ~f:(Set.of_list (module Int))
    in
    List.cartesian_product input input
    |> List.map ~f:(fun (p1, p2) -> area p1 p2)
    |> List.filter_map ~f:(fun (area, ~tl, ~br) ->
      if is_valid red_tiles ~tl ~br
      then (* print_s [%message "valid" (area : int)]; *)
        Some area
      else None)
    |> List.max_elt ~compare
    |> Option.value_exn
  ;;

  module%test [@name "part_2"] _ = struct
    let%expect_test "example" =
      solve example_input |> Common.print_int;
      [%expect {| 24 |}]
    ;;

    let%expect_test "test" =
      let test_input =
        {|

|} |> Common.split_lines |> process_input
      in
      solve test_input |> Common.print_int;
      [%expect {| 24 |}]
    ;;

    (* let%expect_test "prod" =
      let is, js = List.unzip prod_input in
      let is, js = List.dedup_and_sort is ~compare, List.dedup_and_sort js ~compare in
      let has_consecutive list =
        List.fold list ~init:(-1) ~f:(fun last curr ->
          if curr = last + 1
          then (print_s [%message "dup" (curr : int) (last : int)]; curr)
          else curr)
      in
      has_consecutive is |> ignore;
      has_consecutive js |> ignore;
      [%expect {| 4599890450 |}] *)
    ;;
    (* 4599890450 too high *)

    (* let%expect_test "prod" =
      solve prod_input |> Common.print_int;
      [%expect {| 4599890450 |}]
      (* 4599890450 too high *)
    ;; *)
  end
end

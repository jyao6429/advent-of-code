open! Core

let day = "day8"
let example_input = Common.read_lines (day ^ "_example.txt")
let prod_input = Common.read_lines (day ^ ".txt")

module Coordinate = struct
  type t =
    { x : int
    ; y : int
    ; z : int
    }
  [@@deriving sexp_of, compare]

  include functor Comparator.Make

  let t_of_list = function
    | [ x; y; z ] -> { x; y; z }
    | _ -> failwith "invalid"
  ;;

  let float_of_int_t { x; y; z } =
    ~x:(Float.of_int x), ~y:(Float.of_int y), ~z:(Float.of_int z)
  ;;

  let distance t1 t2 =
    let ~x:x1, ~y:y1, ~z:z1 = float_of_int_t t1 in
    let ~x:x2, ~y:y2, ~z:z2 = float_of_int_t t2 in
    ((x1 -. x2) ** 2.) +. ((y1 -. y2) ** 2.) +. ((z1 -. z2) ** 2.) |> Float.sqrt
  ;;
end

module Coordinates = struct
  type t = Set.M(Coordinate).t [@@deriving sexp_of]

  let nearest (t : t) ~coord =
    Set.to_array t
    |> Array.map ~f:(fun target -> Coordinate.distance coord target, target)
    |> Array.min_elt ~compare:[%compare: Float.t * Coordinate.t]
    |> Option.value_exn
  ;;
end

let process_input input =
  let lists =
    List.map input ~f:(fun line -> String.split line ~on:',' |> List.map ~f:Int.of_string)
  in
  List.map lists ~f:Coordinate.t_of_list |> Set.of_list (module Coordinate)
;;

let example_input = example_input |> process_input
let prod_input = prod_input |> process_input

module Part_1 = struct
  let solve coords =
    let _nearest_points =
      Set.to_map coords ~f:(fun coord ->
        Set.remove coords coord |> Coordinates.nearest ~coord)
    in
    (* print_s [%message (nearest_points : (float * Coordinate.t) Map.M(Coordinate).t)]; *)
    0
  ;;

  module%test [@name "part_1"] _ = struct
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

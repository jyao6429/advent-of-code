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
  [@@deriving sexp, compare, equal, hash]

  include functor Comparable.Make
  include functor Hashable.Make

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

module Edge = struct
  type t =
    { distance : float
    ; v1 : Coordinate.t
    ; v2 : Coordinate.t
    }
  [@@deriving sexp, compare]

  include functor Comparable.Make
end

let process_input input =
  let lists =
    List.map input ~f:(fun line -> String.split line ~on:',' |> List.map ~f:Int.of_string)
  in
  let coords = List.map lists ~f:Coordinate.t_of_list in
  let edges =
    List.cartesian_product coords coords
    |> List.filter_map ~f:(fun (c1, c2) ->
      if Coordinate.equal c1 c2
      then None
      else
        Some
          { Edge.distance = Coordinate.distance c1 c2
          ; v1 = Coordinate.min c1 c2
          ; v2 = Coordinate.max c1 c2
          })
    |> Set.of_list (module Edge)
  in
  ~edges, ~coords
;;

let example_input = example_input |> process_input
let prod_input = prod_input |> process_input

module Union_find = struct
  module Node = struct
    type 'a t =
      { mutable parent : 'a
      ; mutable size : int
      }
    [@@deriving sexp_of]
  end

  type t = Coordinate.t Node.t Coordinate.Table.t

  let create coords =
    List.map coords ~f:(fun coord -> coord, { Node.parent = coord; size = 1 })
    |> Hashtbl.of_alist_exn (module Coordinate)
  ;;

  let make_set (t : t) coord =
    Hashtbl.set t ~key:coord ~data:{ Node.parent = coord; size = 1 }
  ;;

  let rec find_set t coord =
    match Hashtbl.find_exn t coord with
    | { Node.parent; _ } as node when Coordinate.equal coord parent -> node
    | { Node.parent; _ } as node ->
      let canon = find_set t parent in
      node.parent <- canon.parent;
      canon
  ;;

  let union t c1 c2 =
    (* print_s
      [%message
        "union"
          (t : Coordinate.t Node.t Coordinate.Table.t)
          (c1 : Coordinate.t)
          (c2 : Coordinate.t)]; *)
    let n1, n2 = find_set t c1, find_set t c2 in
    if [%equal: Coordinate.t] n1.Node.parent n2.Node.parent |> not
    then (
      let smaller, larger = if n1.Node.size < n2.Node.size then n1, n2 else n2, n1 in
      smaller.Node.parent <- larger.Node.parent;
      larger.Node.size <- larger.Node.size + smaller.Node.size)
  ;;
end

module Part_1 = struct
  let solve (~edges, ~coords) ~num_connections =
    let uf = Union_find.create coords in
    let _ =
      List.range 0 num_connections
      |> List.fold ~init:edges ~f:(fun edges _ ->
        let min, edges = Set.min_elt_exn edges, Set.remove_index edges 0 in
        Union_find.union uf min.Edge.v1 min.Edge.v2;
        edges)
    in
    (* print_s
      [%message
        (Hashtbl.to_alist uf : (Coordinate.t * Coordinate.t Union_find.Node.t) list)]; *)
    let circuit_sizes =
      Hashtbl.to_alist uf
      |> List.filter_map ~f:(fun (coord, node) ->
        if Coordinate.equal coord node.parent then Some node.size else None)
    in
    let sorted_decreasing = List.sort circuit_sizes ~compare:(fun a b -> compare b a) in
    let top3 = List.take sorted_decreasing 3 in
    List.fold top3 ~init:1 ~f:( * )
  ;;

  module%test [@name "part_1"] _ = struct
    let%expect_test "example" =
      solve example_input ~num_connections:10 |> Common.print_int;
      [%expect {| 40 |}]
    ;;

    let%expect_test "prod" =
      solve prod_input ~num_connections:1000 |> Common.print_int;
      [%expect {| 75582 |}]
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

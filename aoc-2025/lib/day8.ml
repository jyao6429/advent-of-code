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
  ~edges, ~num_coords:(List.length coords)
;;

let example_input = example_input |> process_input
let prod_input = prod_input |> process_input

module Union_find = struct
  module Node = struct
    type 'a canon =
      { size : int
      ; value : 'a
      }
    [@@deriving sexp_of]

    type 'a child = { mutable parent : 'a
          ; value : 'a
          } [@@deriving sexp_of]

    type 'a t =
      | Canonical of 'a canon
      | Child of 'a child
          
    [@@deriving sexp_of]
  end

  type t = Coordinate.t Node.t Coordinate.Table.t

  let create = Coordinate.Table.create

  let singleton (t : t) value =
    Hashtbl.update_and_return
      t
      value
      ~f:(Option.value ~default:(Node.Canonical { size = 1; value }))
  ;;

  let canonical t = function
    | Node.Canonical _ as node -> node
    | Child c ->
      let rec loop node acc =
        match node with
        | Node.Canonical canon ->
          List.iter acc ~f:(fun
             c -> c.Node.parent <- canon.value
            );
          node
        | Child c -> loop (Hashtbl.find_exn t c.parent) (c :: acc)
      in
      loop (Child c) []
  ;;

  let union t n1 n2 =
    match n1, n2 with
    | Node.Canonical can1, Node.Canonical can2 ->
      print_s
        [%message
          "unioning" (can1 : Coordinate.t Node.canon) (can2 : Coordinate.t Node.canon)];
      if can1.size < can2.size
      then
        Hashtbl.set
          t
          ~key:can1.value
          ~data:(Node.Child { parent = can2.value; value = can1.value })
      else
        Hashtbl.set
          t
          ~key:can2.value
          ~data:(Node.Child { parent = can1.value; value = can2.value })
    | _ -> failwith "invalid"
  ;;
end

module Part_1 = struct
  let solve (~edges, ~num_coords) =
    let uf = Union_find.create () in
    let _ =
      List.range 0 num_coords
      |> List.fold ~init:edges ~f:(fun edges _ ->
        let min, edges = Set.min_elt_exn edges, Set.remove_index edges 0 in
        let can1, can2 =
          ( Union_find.singleton uf min.Edge.v1 |> Union_find.canonical uf
          , Union_find.singleton uf min.Edge.v2 |> Union_find.canonical uf )
        in
        Union_find.union uf can1 can2;
        edges)
    in
    print_s
      [%message
        (Hashtbl.to_alist uf : (Coordinate.t * Coordinate.t Union_find.Node.t) list)];
    let circuit_sizes =
      Hashtbl.to_alist uf
      |> List.filter_map ~f:(function
        | _, Union_find.Node.Canonical { size; _ } -> Some size
        | _ -> None)
    in
    let sorted_decreasing = List.sort circuit_sizes ~compare:(fun a b -> compare b a) in
    let top3 = List.take sorted_decreasing 3 in
    List.fold top3 ~init:1 ~f:( * )
  ;;

  module%test [@name "part_1"] _ = struct
    let%expect_test "example" =
      solve example_input |> Common.print_int;
      [%expect {| 1 |}]
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

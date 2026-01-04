open! Core

let day = "day10"
let example_input = Common.read_lines (day ^ "_example.txt")
let prod_input = Common.read_lines (day ^ ".txt")

let process_indicator_lights str =
  String.strip str ~drop:(fun c -> Char.equal c '[' || Char.equal c ']')
  |> String.to_list
  |> List.map ~f:(function
    | '.' -> false
    | '#' -> true
    | _ -> failwith "invalid")
;;

let process_buttons str_list =
  List.map str_list ~f:(fun str ->
    String.strip str ~drop:(fun c -> Char.equal c '(' || Char.equal c ')')
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
    |> Set.of_list (module Int))
;;

let process_joltage str =
  String.strip str ~drop:(fun c -> Char.equal c '{' || Char.equal c '}')
  |> String.split ~on:','
  |> Iarray.of_list_map ~f:Int.of_string
;;

let process_input input =
  let partitioned =
    List.map input ~f:(String.split ~on:' ')
    |> List.map ~f:(function
      | [] -> failwith "invalid input"
      | indicator_lights :: rest ->
        let buttons, joltage = List.drop_last_exn rest, List.last_exn rest in
        ( process_indicator_lights indicator_lights
        , process_buttons buttons
        , process_joltage joltage ))
  in
  partitioned
;;

let example_input = example_input |> process_input
let prod_input = prod_input |> process_input

module Part_1 = struct
  let rec brute_force_min_presses
    ~target_indicator_lights
    ~current_indicator_lights
    ~remaining_buttons
    ~num_buttons_pressed
    =
    if [%equal: bool list] target_indicator_lights current_indicator_lights
    then num_buttons_pressed
    else (
      match remaining_buttons with
      | [] -> Int.max_value
      | button :: rest ->
        let count_if_no_press =
          brute_force_min_presses
            ~target_indicator_lights
            ~current_indicator_lights
            ~remaining_buttons:rest
            ~num_buttons_pressed
        in
        let new_indicator_lights =
          List.mapi current_indicator_lights ~f:(fun i state ->
            if Set.mem button i then not state else state)
        in
        let count_if_pressed =
          brute_force_min_presses
            ~target_indicator_lights
            ~current_indicator_lights:new_indicator_lights
            ~remaining_buttons:rest
            ~num_buttons_pressed:(num_buttons_pressed + 1)
        in
        Int.min count_if_no_press count_if_pressed)
  ;;

  let find_min_presses (indicator_lights, buttons, _) =
    brute_force_min_presses
      ~target_indicator_lights:indicator_lights
      ~current_indicator_lights:(List.create false ~len:(List.length indicator_lights))
      ~remaining_buttons:buttons
      ~num_buttons_pressed:0
  ;;

  let solve input = List.sum (module Int) input ~f:find_min_presses

  module%test [@name "part_1"] _ = struct
    let%expect_test "example" =
      solve example_input |> Common.print_int;
      [%expect {| 7 |}]
    ;;

    let%expect_test "prod" =
      solve prod_input |> Common.print_int;
      [%expect {| 522 |}]
    ;;
  end
end

module Part_2 = struct
  let find_min_presses (_, buttons, target_joltage) =
    let equations = Array.init (Iarray.length target_joltage) ~f:(fun _ -> []) in
    let vars =
      List.mapi buttons ~f:(fun i button ->
        let var = Lp.var ~integer:true ("i" ^ Int.to_string i) in
        Set.iter button ~f:(fun idx -> equations.(idx) <- var :: equations.(idx));
        var)
    in
    let sum = List.reduce vars ~f:Lp.( ++ ) |> Option.value_exn in
    let constraints =
      Array.to_list equations
      |> List.mapi ~f:(fun i vars ->
        let var_sum = List.reduce vars ~f:Lp.( ++ ) |> Option.value_exn in
        Lp.( =~ ) var_sum (Lp.c (float_of_int target_joltage.:(i))))
    in
    let problem = Lp.make (Lp.minimize sum) constraints in
    match Lp_glpk.solve ~term_output:false problem with
    | Ok (obj, _) -> Int.of_float obj
    | Error msg -> failwith msg
  ;;

  let solve (input : (bool list * Int.Set.t list * int iarray) list @ portable) =
    List.sum (module Int) input ~f:find_min_presses
  ;;

  module%test [@name "part_2"] _ = struct
    let%expect_test "example" =
      solve example_input |> Common.print_int;
      [%expect {| 33 |}]
    ;;

    let%expect_test "prod" =
      solve prod_input |> Common.print_int;
      [%expect {| 18105 |}]
    ;;
  end
end

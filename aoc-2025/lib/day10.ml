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
  let rec brute_force_min_presses
    ~target_joltage
    ~current_joltage
    ~remaining_buttons
    ~num_presses
    =
    if [%equal: int iarray] target_joltage current_joltage
    then num_presses
    else (
      match remaining_buttons with
      | [] -> Int.max_value
      | button :: rest ->
        let max_presses_for_button =
          Set.to_list button
          |> List.map ~f:(fun i -> target_joltage.:(i) - current_joltage.:(i))
          |> List.min_elt ~compare
          |> Option.value_exn
          |> Int.clamp_exn ~min:0 ~max:Int.max_value
        in
        List.range ~stop:`inclusive 0 max_presses_for_button
        |> List.map ~f:(fun presses ->
          let new_joltage =
            Iarray.mapi current_joltage ~f:(fun i value ->
              if Set.mem button i then value + presses else value)
          in
          brute_force_min_presses
            ~target_joltage
            ~current_joltage:new_joltage
            ~remaining_buttons:rest
            ~num_presses:(num_presses + presses))
        |> List.min_elt ~compare
        |> Option.value_exn)
  ;;

  let find_min_presses (_, buttons, target_joltage) =
    brute_force_min_presses
      ~target_joltage
      ~current_joltage:
        (Iarray.create 0 ~len:(Iarray.length target_joltage) ~mutate:ignore)
      ~remaining_buttons:buttons
      ~num_presses:0
  ;;

  let solve input = List.sum (module Int) input ~f:find_min_presses

  module%test [@name "part_2"] _ = struct
    let%expect_test "example" =
      solve example_input |> Common.print_int;
      [%expect {| 33 |}]
    ;;

    (* let%expect_test "prod" =
      solve prod_input |> Common.print_int;
      [%expect {| 0 |}]
    ;; *)
  end
end

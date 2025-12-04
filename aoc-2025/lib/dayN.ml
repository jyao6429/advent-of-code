open! Core

let day = "dayN"
let process_input input = input

let example_input = Common.read_lines (day ^ "_example.txt") |> process_input

let prod_input = Common.read_lines (day ^ ".txt") |> process_input

module Part_1 = struct
  let solve input =
    ignore input;
    0
  ;;

  module%test [@name "part_1"] _ = struct
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

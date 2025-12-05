open! Core

let day = "day5"

let example_input = Common.read_lines (day ^ "_example.txt")

let prod_input = Common.read_lines (day ^ ".txt")
let process_input input = input
let example_input = example_input |> process_input
let prod_input = prod_input |> process_input

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

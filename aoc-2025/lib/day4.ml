open! Core

let example_input = Common.read_lines (day ^ "_example.txt")

let prod_input = Common.read_lines "day4.txt"

module Part_1 = struct
  let solve input =
    ignore input;
    0
  ;;

  let%expect_test "part 1 - example" =
    solve example_input |> Common.print_int;
    [%expect {| 0 |}]
  ;;

  let%expect_test "part 1 - prod" =
    solve prod_input |> Common.print_int;
    [%expect {| 0 |}]
  ;;
end

module Part_2 = struct
  let solve input =
    ignore input;
    0
  ;;

  let%expect_test "part 2 - example" =
    solve example_input |> Common.print_int;
    [%expect {| 0 |}]
  ;;

  let%expect_test "part 2 - prod" =
    solve prod_input |> Common.print_int;
    [%expect {| 0 |}]
  ;;
end

open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9

(* YOUR TESTS HERE *)

let%test "test_eval_2" = parse "1 + ((6 + 3) + (1 + 2))" |> eval = Ok 13
let%test "test_eval_3" = parse "1 + ((6 + 3) + (10 + 2))" |> eval = Ok 22

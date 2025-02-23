open Boolexpr.Main
open Boolexpr.Ast

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

let test_trace expr exp_result =
  (expr |> parse |> trace) = exp_result

(* ### Unit tests for task 4 *)

let%test "test_eval_1" = test_eval "false" false;;
let%test "test_eval_2" = test_eval "true" true;;
let%test "test_eval_3" = test_eval "if true then false else true" false;;
let%test "test_eval_4" = test_eval "if false then false else true" true;;
let%test "test_eval_5" = test_eval "if true then (if true then false else true) else (if true then true else false)" false;;
let%test "test_eval_6" = test_eval "if (if false then false else false) then (if false then true else false) else (if true then false else true)" false;;
let%test "test_eval_7" = test_eval "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" false;;

(* ### Unit tests for task 5 *)

let%test "test_trace1_1" = let expr = "if true then true else false" in try trace1 (parse expr) = True with NoRuleApplies -> is_value (parse expr) = true;;
let%test "test_trace1_2" = let expr = "if false then true else false" in try trace1 (parse expr) = False with NoRuleApplies -> is_value (parse expr) = true;;
let%test "test_trace1_3" = let expr = "false" in try trace1 (parse expr) = False with NoRuleApplies -> is_value (parse expr) = true;;
let%test "test_trace" =
"if (if false then false else false) then (if false then true else false) else (if true then false else true)"
 |> parse |> trace |> List.length <= 10;;

(* ### Unit tests for task 6 *)
let%test "test_trace_1" = let expr = "true && if true then true else false" in
  parse expr |> trace |> List.rev |> List.hd = True;;

let%test "test_trace_2" = let expr = "true && if true then false else true" in
  parse expr |> trace |> List.rev |> List.hd = False;;

let%test "test_trace_3" = let expr = "true || if true then false else true" in
  parse expr |> trace |> List.rev |> List.hd = True;;
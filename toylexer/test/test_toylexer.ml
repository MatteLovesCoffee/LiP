open Toylexer.Token
open Toylexer.Main

let%test "test_frequencies_1" =
  lexer "x=y; x=x+1" |> frequency 3 = [(ID "x", 3); (ASSIGN, 2); (ID "y", 1)]

(* YOUR TESTS HERE *)

let%test "test_ATOK" =
  lexer "Class class Class" |> frequency 3 = [(ATOK "Class", 2)];;
  
let%test "test_BTOK" =
  lexer "aa aa aa aa" |> frequency 3 = [(BTOK "aa", 4)];;
    
let%test "test_CTOK" =
  lexer "cat cat cat" |> frequency 3 = [(CTOK "cat", 3)];;

let%test "test_DTOK" =
  lexer "-3.14 -3.14 3.14 3.14 -3.14" |> frequency 3 = [(DTOK "-3.14", 3); (DTOK "3.14", 2)];;
    
let%test "test_ETOK" =
  lexer "0x44 0x44 0x44" |> frequency 3 = [(ETOK "0x44", 3)];;
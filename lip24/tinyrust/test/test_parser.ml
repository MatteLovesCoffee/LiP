open TinyrustLib
open Common

(** ------------------------------------------
    Start of parser tests
    ------------------------------------------ *)

let%test_unit "test_parser" =
  Array.iter
    (fun ex ->
      let p = read_file ex in
      try
        Trace.parse p |> ignore;
        pr "✔ %s\n" ex
      with _ ->
        pr "✘ Couldn't parse %s\n" ex)
    examples

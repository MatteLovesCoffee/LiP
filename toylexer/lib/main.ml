open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n tlist =
  let rec firstn n = function
    [] -> []
  | x::t -> if n=1 then [x] else x::firstn (n-1) t
in
firstn n (List.rev (List.sort (fun a b -> let occ = compare (snd a) (snd b) in if occ=0 then compare (fst a) (fst b) else occ) (List.fold_left (fun acc token -> let element = (token, List.length (List.filter (fun a -> token = a) tlist)) in if (List.mem element acc) then acc else acc@[element]) [] tlist)))

let rec lang1 w = 
  match w with
    [] -> false |
    '0' :: ls -> lang1 ls |
    '1' :: ls -> lang1 ls |
    _ -> false

let lang2 _ = failwith ""

let lang3 _ = failwith ""

let lang4 _ = failwith ""

let lang5 _ = failwith ""
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  

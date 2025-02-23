type ast =
    Const of int
  | Times of ast * ast
  | Div of ast * ast
  | Add of ast * ast
  | Sub of ast * ast
type expr =
  | Vide
  | Eps
  | L of int
  | Somme of expr * expr
  | Concat of expr * expr
  | Etoile of expr

let rec expr_reduite e =
  match e with
  | Somme (a, b) -> (
      match (expr_reduite a, expr_reduite b) with
      | Vide, y | y, Vide -> y
      | x, y -> Somme (x, y))
  | Concat (a, b) -> (
      match (expr_reduite a, expr_reduite b) with
      | Vide, _ | _, Vide -> Vide
      | x, y -> Concat (x, y))
  | Etoile Vide -> Eps
  | _ -> e

let ex1 = Concat (Somme (Etoile Vide, Vide), L 1)

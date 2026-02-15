let reines n =
  let tab = Array.make n (-1) in
  let count = ref 0 in

  let calculedisponible k tab =
    let n = Array.length tab in
    let dispo = Array.make n true in
    tab
    |> Array.iteri (fun i j ->
           if i < k then (
             let offset = k - i in
             dispo.(j) <- false;
             if j - offset >= 0 then dispo.(j - offset) <- false;
             if j + offset < n then dispo.(j + offset) <- false));
    dispo
  in

  let rec essai k =
    let n = Array.length tab in
    if k = n then incr count
    else
      calculedisponible k tab
      |> Array.iteri (fun j free ->
             if free then (
               tab.(k) <- j;
               essai (k + 1)))
  in
  essai 0;
  (!count, tab)

(* Exercice 2 *)
type terme =
  | V
  | F
  | Var of int
  | Non of terme
  | Et of terme * terme
  | Ou of terme * terme

let p = Var 0
and q = Var 1
and r = Var 2

let ( ++ ) f g = Ou (f, g)
let ( ** ) f g = Et (f, g)
let ( => ) f g = Ou (Non f, g)
let ( <=> ) f g = Ou (Et (f, g), Et (Non f, Non g))
let f1 = p => q <=> (Non q => Non p)

let rec varmax f =
  match f with
  | V | F -> (0, -1)
  | Var x -> (0, x)
  | Non t ->
      let max_t, var_t = varmax t in
      (max_t + 1, var_t)
  | Et (l, r) | Ou (l, r) ->
      let max_l, var_l = varmax l and max_r, var_r = varmax r in
      if max_l > max_r then (max_l + 1, var_l) else (max_r + 1, var_r)

let rec subs i f t =
  match t with
  | Var x when x = i -> f
  | V | F | Var _ -> t
  | Non x -> Non (subs i f x)
  | Et (l, r) -> Et (subs i f l, subs i f r)
  | Ou (l, r) -> Ou (subs i f l, subs i f r)

let rec simplifie = function
  | (Ou (v, _) | Ou (_, v)) when simplifie v = V -> V
  | (Ou (f, t) | Ou (t, f)) when simplifie f = F -> t
  | (Et (f, _) | Et (_, f)) when simplifie f = F -> F
  | (Et (v, t) | Et (t, v)) when simplifie v = V -> t
  | Non v when simplifie v = V -> F
  | Non f when simplifie f = F -> V
  | Non f -> Non (simplifie f)
  | Ou (l, r) -> Ou (simplifie l, simplifie r)
  | Et (l, r) -> Et (simplifie l, simplifie r)
  | t -> t

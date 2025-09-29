let eg = [| 8; 7; 3; 9; 14; 15; 6; 1 |]

(* Exercice 1 *)
let amplitude t =
  let maxi = ref t.(0) and mini = ref t.(0) in
  for i = 1 to Array.length t - 1 do
    if t.(i) > !maxi then maxi := t.(i) else if t.(i) < !mini then mini := t.(i)
  done;
  !maxi - !mini

let gain t =
  let rec gain_rec t start_index end_index : int * int * int =
    if start_index = end_index then (0, t.(start_index), t.(start_index))
    else
      let mid = (start_index + end_index) / 2 in
      let g1, min1, max1 = gain_rec t start_index mid
      and g2, min2, max2 = gain_rec t (mid + 1) end_index in
      (max (max2 - min1) (max g1 g2), min min1 min2, max max1 max2)
  in
  let g, _, _ = gain_rec t 0 (Array.length t - 1) in
  g
;;

amplitude eg;;
gain eg

(* gi, current_max, in day, out day *)
let gain2 t =
  let rec gi t i : int * int * int * int =
    if i = 0 then (0, 0, 0, 0)
    else
      let g, current_max, in_day, out_day = gi t (i - 1) in
      let next_alg = g + t.(i) - t.(i - 1) in
      if next_alg < 0 then
        if current_max > 0 then (0, current_max, in_day, out_day)
        else (0, 0, i, i)
      else if next_alg > current_max then (next_alg, next_alg, in_day, i)
      else (next_alg, current_max, in_day, out_day)
  in
  let _, g, i, o = gi t (Array.length t - 1) in
  (g, i, o)
;;

gain2 eg

let two_max t =
  let a, b = (t.(0), t.(1)) in
  let max_1 = ref (max a b) and max_2 = ref (min a b) in
  for i = 2 to Array.length t - 1 do
    let c = t.(i) in
    if c > !max_1 then (
      max_2 := !max_1;
      max_1 := c)
    else if c > !max_2 then max_2 := c
  done;
  (max_1, max_2)

(* Exercice 2 *)
let two_max_2 t =
  let rec max_slice t i j =
    if j - i = 1 then if t.(i) > t.(j) then (t.(i), t.(j)) else (t.(j), t.(i))
    else
      let mid = (i + j) / 2 in
      let a, b = max_slice t i mid and c, d = max_slice t (mid + 1) j in
      if a > c then (a, max c b) else (c, max a d)
  in
  max_slice t 0 (Array.length t - 1)
;;

two_max_2 eg

let two_max_3 t =
  let rec tournament t i j : int * int list =
    if j = i then (t.(i), [])
    else
      let mid = (i + j) / 2 in
      let max1, a1 = tournament t i mid
      and max2, a2 = tournament t (mid + 1) j in
      if max1 > max2 then (max1, max2 :: a1) else (max2, max1 :: a2)
  in
  let m, a = tournament t 0 (Array.length t - 1) in
  (m, a)

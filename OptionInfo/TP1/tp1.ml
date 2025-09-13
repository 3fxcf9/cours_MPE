(* Exercice 1 *)
let rec bits_1 (n : int) : int list =
  let bits = ref [] and q = ref n in
  while !q > 0 do
    bits := (!q mod 2) :: !bits;
    q := !q / 2
  done;
  !bits

let rec bits_2 (n : int) : int list =
  if n = 0 then [] else (n mod 2) :: bits_1 (n / 2)

let rec insert (l : 'a list) (x : 'a) =
  match l with
  | [] -> x :: []
  | h :: _ when x <= h -> x :: l
  | h :: q -> h :: insert q x

let rec insertion_sort (l : 'a list) : 'a list =
  match l with [] -> [] | x :: q -> insert (insertion_sort q) x

let insertion_sort_array (arr : 'a array) =
  for k = 1 to Array.length arr - 1 do
    let i = ref (k - 1) and current = arr.(k) in
    while !i >= 0 && current < arr.(!i) do
      arr.(!i + 1) <- arr.(!i);
      decr i
    done;
    arr.(!i + 1) <- current
  done

open Printf;;

insertion_sort [ 3; 1; 8; 0 ] |> List.iter (printf "%d ")

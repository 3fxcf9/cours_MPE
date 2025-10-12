(* Exercice 4 *)

let print_matrix matrix =
  Array.iter
    (fun row ->
      Array.iter
        (fun elem ->
          Printf.printf "%d "
            elem (* Change %d to %f for floating-point numbers *))
        row;
      print_newline ())
    matrix

let palindrome mot =
  let n = String.length mot in
  let l = Array.make_matrix n n 0 in
  let s = Array.make_matrix n n 0 in
  for i = n - 1 downto 0 do
    for j = i to n - 1 do
      l.(i).(j) <-
        (if i = j then 1
         else if mot.[i] <> mot.[j] then
           if l.(i + 1).(j) < l.(i).(j - 1) then (
             s.(i).(j) <- 0;
             l.(i).(j - 1))
           else (
             s.(i).(j) <- 1;
             l.(i + 1).(j))
         else (
           s.(i).(j) <- 2;
           l.(i + 1).(j - 1) + 2))
    done
  done;
  (* print_matrix l; *)
  print_matrix s;
  (l.(0).(n - 1), s)

let rec fabrique s m i j =
  if i = j then Char.escaped m.[i]
  else
    match s.(i).(j) with
    | 0 -> fabrique s m i (j - 1)
    | 1 -> fabrique s m (i + 1) j
    | _ ->
        Char.escaped m.[i] ^ fabrique s m (i + 1) (j - 1) ^ Char.escaped m.[j]

(* let m = "mathématiques" *)
let m = "xyzxy"
let _, s = palindrome m
let r = fabrique s m 0 (String.length m - 1)

(* Exercice 3 *)

let profit n l p =
  let s = Array.length l in
  let r = Array.make (n + 1) 0 in
  let c = Array.make (n + 1) (-1) in
  for j = 1 to n do
    let i = ref 0 in
    while !i < s && l.(!i) <= j do
      if r.(j) < p.(!i) + r.(j - l.(!i)) then (
        r.(j) <- p.(!i) + r.(j - l.(!i));
        c.(j) <- !i);
      incr i
    done
  done;
  r.(n)

let decoupe n c l =
  let reste = ref n in
  let decoupes = Array.make (Array.length l) 0 in
  while c.(!reste) >= 0 do
    let k = c.(!reste) in
    decoupes.(k) <- decoupes.(k) + 1;
    reste := !reste - l.(k)
  done;
  decoupes

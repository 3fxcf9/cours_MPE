let biparti (g : int list array) : bool * int array =
  let n = Array.length g in
  let q = ref [] in
  let partition = Array.make n (-1) in
  let est_biparti = ref true in
  for i = 0 to n - 1 do
    if partition.(i) < 0 && !est_biparti then (
      partition.(i) <- 0;
      q := i :: !q;
      while !q <> [] do
        let (current :: rest) = !q in
        q := rest;
        List.iter
          (fun vois ->
            match partition.(vois) with
            | -1 ->
                partition.(vois) <- 1 - partition.(current);
                q := !q @ [ current ]
            | k when k = partition.(current) -> est_biparti := false
            | _ -> ())
          g.(i)
      done)
  done;
  (!est_biparti, partition)

let g1 = [| [ 3; 4 ]; [ 3; 4; 5 ]; [ 6 ]; [ 0; 1 ]; [ 0; 1 ]; [ 1 ]; [ 2 ] |]

let verifie_couplage vadj c =
  let ok = ref true in
  let i = ref 0 in
  while !i < Array.length vadj && !ok do
    if c.(!i) <> -1 then (
      let j = c.(!i) in
      ok := !ok && c.(j) = !i && List.mem j vadj.(!i);
      incr i)
  done;
  !ok

let rec cherche vadj c m x =
  let l = ref vadj.(x) in
  let res = ref - 1 in

  while !l <> [] && !res = -1 do
    let y = List.hd !l in
    l := List.tl !l;
    if c.(y) = -1 then (
      res := !y;
      m.(y) <- x)
    else if m.(y) = -1 then (
      m.(y) <- x;
      m.(c.(y)) <- y;
      res := cherche vadj c m c.(y))
  done;
  !res

let rec actualise c m s =
  if s <> -1 then (
    c.(s) <- m.(s);
    c.(m.(s)) <- s;
    actualise c m m.(m.(s)))

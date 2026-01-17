(* exercice 1 *)
let adj_list (arcs : (int * int) list) (n : int) =
  let adj = Array.make n [] in
  arcs |> List.iter (fun (from, t) -> adj.(from) <- t :: adj.(from));
  adj

let coadjacence (adj : int list array) =
  let n = Array.length adj in
  let coadj = Array.make n [] in
  adj
  |> Array.iteri (fun i neighbors ->
         List.iter (fun j -> coadj.(j) <- i :: coadj.(j)) neighbors);
  coadj

let a = [ (1, 2); (2, 3); (2, 4); (1, 4) ]

(* exercice 2 *)
let composante_connexe (graph : int list array) =
  let n = Array.length graph in
  let visited = Array.make n (-1) in
  let count = ref 0 in
  for i = 0 to n - 1 do
    if visited.(i) < 0 then (
      let rec explore s =
        if visited.(s) < 0 then (
          visited.(s) <- !count;
          List.iter explore graph.(s))
      in
      explore i;
      incr count)
  done;
  visited

let b = [| [ 1 ]; [ 0; 2; 3 ]; [ 1 ]; [ 4; 1 ]; [ 3 ]; [] |] (* cycle *)
let c = [| [ 1 ]; [ 0; 2; 3 ]; [ 1 ]; [ 1 ] |] (* no cycle *)
let d = [| []; [ 2 ]; [ 1; 3; 4 ]; [ 2 ]; [ 5; 2 ]; [ 4 ]; [] |] (* cycle *)

(* exercice 3 *)

let cycle (graph : int list array) =
  let n = Array.length graph in
  let visited_from = Array.make n (-1) in
  let rec explore from s =
    if visited_from.(s) < 0 then (
      visited_from.(s) <- from;
      graph.(s)
      |> List.fold_left
           (fun acc cur ->
             acc
             || (visited_from.(cur) > 0 && visited_from.(cur) <> s)
             || explore s cur)
           false)
    else false
  in
  List.exists (explore (-1)) (List.init n Fun.id)

(* exercice 4 *)

let topological_sort (graph : int list array) =
  let n = Array.length graph in
  let visited = Array.make n false in
  let acc = ref [] in
  for i = 0 to n - 1 do
    if not visited.(i) then
      let rec explore s =
        if not visited.(s) then (
          visited.(s) <- true;
          List.iter explore graph.(s);
          acc := s :: !acc)
      in
      explore i
  done;
  acc

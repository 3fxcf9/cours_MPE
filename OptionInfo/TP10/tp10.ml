let comp v1 v2 = max (-1) @@ min (v2 - v1) 1

let echange arr a b =
  let temp = arr.(a) in
  arr.(a) <- arr.(b);
  arr.(b) <- temp

let verifie_tas tas =
  let res = ref true in
  for i = 2 to tas.(0) do
    if compare tas.(i) tas.(i / 2) > -1 then res := false
  done;
  res

let rec entasser i tas =
  let n = tas.(0) in
  let max_idx = ref i in
  if 2 * i <= n && comp tas.(!max_idx) tas.(2 * i) = 1 then max_idx := 2 * i;
  if (2 * i) + 1 <= n && comp tas.(!max_idx) tas.((2 * i) + 1) = 1 then
    max_idx := (2 * i) + 1;
  if !max_idx <> i then begin
    echange tas i !max_idx;
    entasser !max_idx tas
  end

let construiretas arb =
  (* dernier nœud avec un fils = père du dernier nœud *)
  for i = arb.(0) / 2 downto 1 do
    entasser i arb
  done

let trier arr =
  arr.(0) <- Array.length arr - 1;
  construiretas arr;
  for i = arr.(0) downto 2 do
    echange arr i 1;
    arr.(0) <- arr.(0) - 1;
    entasser 1 arr
  done

let a = [| 0; 4; 2; 3; 5 |]

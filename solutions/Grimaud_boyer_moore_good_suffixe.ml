(* Function to compute the suffixes array *)
let suffixes pattern =
  let m = String.length pattern in
  let suff = Array.make m 0 in
  let f = ref 0 in  (* Stores the starting position of the matched suffix *)
  let g = ref (m - 1) in  (* Tracks the end position of the matched suffix *)

  (* Initialize the last element *)
  suff.(m - 1) <- m;

  (* Process the pattern from right to left *)
  for i = m - 2 downto 0 do
    (* Case 1: Use previously computed values *)
    if i > !g && suff.(i + m - 1 - !f) < i - !g then
      suff.(i) <- suff.(i + m - 1 - !f)
    else begin
      (* Case 2: Compute suffix values explicitly *)
      f := i;
      g := min !g i;
      (* Expand the suffix match as far as possible *)
      while !g >= 0 && pattern.[!g] = pattern.[!g + m - 1 - !f] do
        g := !g - 1
      done;
      (* Store the length of the matched suffix *)
      suff.(i) <- !f - !g
    end
  done;
  suff


(* Function to construct the good_suffix table *)
let good_suffix_table pattern =
  let m = String.length pattern in
  let suff = suffixes pattern in

  (* Fill the good_suffix table with default values *)
  let good_suffix = Array.make m m in

  (* Fill the table based on suffixes *)
  let j = ref 0 in
  for i = m - 1 downto 0 do
    if suff.(i) = i + 1 then
      while !j < m - 1 - i do
        if good_suffix.(!j) = m then good_suffix.(!j) <- m - 1 - i;
        j := !j + 1
      done
  done;

  for i = 0 to m - 2 do
    good_suffix.(m - 1 - suff.(i)) <- m - 1 - i
  done;
  good_suffix

(* Boyer-Moore algorithm using only the good suffix strategy *)
let boyer_moore_good_suffix text pattern =
  let n = String.length text in
  let m = String.length pattern in
  let good_suffix = good_suffix_table pattern in

  let rec search i =
    (* If the pattern is not found *)
    if i > n - m then -1
    else
      let rec match_suffix j =
        (* If the pattern is found *)
        if j < 0 then i
        else if text.[i + j] = pattern.[j] then
          match_suffix (j - 1)
        else
          let shift = good_suffix.(j) in
          begin
          Printf.printf "%d\n" shift;
          search (i + shift)
            end
      in
      match_suffix (m - 1)
  in
  search 0
    
let () =
  if Array.length Sys.argv < 3 then
    Printf.printf "Usage: %s <text> <pattern>\n" Sys.argv.(0)
  else
    let text = Sys.argv.(1) in
      let pattern = Sys.argv.(2) in
    (*let text = "GCTAAGCAGGATGACTGTCTGACTGTCTGA" in
      let pattern = "CTGTCTGA" in*)
    let pos = boyer_moore_good_suffix text pattern in
    match pos with
    | -1 -> Printf.printf "Pattern \"%s\" not found.\n" pattern
    | _ -> Printf.printf "Pattern \"%s\" found at position %d.\n" pattern pos
         

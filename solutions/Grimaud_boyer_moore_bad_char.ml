let max_characters = 256

let bad_char_shift pattern =
  let m = String.length pattern in
  let table = Hashtbl.create max_characters in

  (* Initialize the table with arrays of -1 *)
  String.iter (fun c ->
    let arr = Array.make m (-1) in
    Hashtbl.add table c arr
  ) pattern;

  (* Update the arrays with the last positions *)
  for j = 0 to m - 1 do
    let c = pattern.[j] in
    let arr = Hashtbl.find table c in
    
    for v = j+1 to m-1 do
        arr.(v) <- j
    done
  done;
  table

let boyer_moore_bad_char text pattern =
  let n = String.length text in
  let m = String.length pattern in
  let table = bad_char_shift pattern in 
  let rec search i =
    (* pattern not found *)
    if i > n-m then
      -1
    else
      let rec search_pattern j =
        (* pattern found at i *)
        if j<0 then
          i
        else
        if pattern.[j] = text.[i+j] then
            search_pattern (j-1)
        else
          let k = (Hashtbl.find table text.[i+j]).(j) in
          let shift = (j-k) in
          search (i+shift)
      in
      search_pattern (m-1)
  in
  search 0

let boyer_moore_bad_char_all text pattern =
  let n = String.length text in
  let m = String.length pattern in
  let table = bad_char_shift pattern in

  let rec search i acc =
    (* if the index is out of range, return acc *)
    if i > n - m then
      List.rev acc
    else
      let rec search_pattern j =
        if j < 0 then
          (* pattern found at i *)
          search (i + 1) (i :: acc)
        else if pattern.[j] = text.[i + j] then
          search_pattern (j - 1)
        else
          let k =
            try (Hashtbl.find table text.[i + j]).(j)
            with Not_found -> -1 
          in
          let shift = j - k in
          search (i + shift) acc
      in
      search_pattern (m - 1)
  in
  search 0 []
   


let () =
  if Array.length Sys.argv < 3 then
    Printf.printf "Usage: %s <text> <pattern>\n" Sys.argv.(0)
  else
    let text = Sys.argv.(1) in
    let pattern = Sys.argv.(2) in
    (*let text = "GCTAAGCAGGATGACTGTCTGACTGTCTGA" in
      let pattern = "CTGTCTGA" in*)
    let pos = boyer_moore_bad_char text pattern in
    match pos with
    | -1 -> Printf.printf "Pattern \"%s\" not found.\n" pattern
    | _ -> Printf.printf "Pattern \"%s\" found at position %d.\n" pattern pos
             

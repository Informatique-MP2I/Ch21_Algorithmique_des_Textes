(* Simple hash function: sum of the ASCII codes of the characters *)
let hash s =
  let len = String.length s in
  let rec aux i acc =
    if i >= len then acc
    else aux (i + 1) (acc + Char.code s.[i])
  in
  aux 0 0

(* Function to update the hash for the next substring *)
let rolling_hash current_hash old_char new_char =
  current_hash - Char.code old_char + Char.code new_char


let rabin_karp text pattern =
  let n = String.length text in
  let m = String.length pattern in
  
  let pattern_hash = hash pattern in
  let initial_hash = hash (String.sub text 0 m) in
  
  let rec search i current_hash =
    if i > n - m then -1
    else
    if current_hash = pattern_hash && (String.sub text i m) = pattern then i
    else
      let next_hash =
        if i < n - m then
          rolling_hash current_hash text.[i] text.[i + m]
        else current_hash
      in
      search (i + 1) next_hash
  in
  search 0 initial_hash




let () =
  if Array.length Sys.argv < 3 then
    Printf.printf "Usage: %s <text> <pattern>\n" Sys.argv.(0)
  else
    let text = Sys.argv.(1) in
    let pattern = Sys.argv.(2) in
    (*let text = "GCTAAGCAGGATGACTGTCTGACTGTCTGA" in
      let pattern = "CTGTCTGA" in*)
    let pos = rabin_karp text pattern in
    match pos with
    | -1 -> Printf.printf "Pattern \"%s\" not found.\n" pattern
    | _ -> Printf.printf "Pattern \"%s\" found at position %d.\n" pattern pos
         


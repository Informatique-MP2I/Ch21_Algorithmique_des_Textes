(* Hash function: sum of the ASCII codes of the characters *)
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

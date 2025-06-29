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


let false_positive_count = ref 0

let rabin_karp_all text pattern =
  let n = String.length text in
  let m = String.length pattern in
  if m > n then
    []
  else
    let pattern_hash = hash pattern in
    let initial_hash = hash (String.sub text 0 m) in
    
    let rec search i current_hash acc =
      if i > n - m then List.rev acc
      else
        let next_acc = 
          if current_hash = pattern_hash then
            if (String.sub text i m) = pattern then
              i :: acc
            else
              begin
                (* Increment false positive count *)
                incr false_positive_count;
                acc
              end
          else
            acc
        in
        let next_hash =
          if i < n - m then
            rolling_hash current_hash text.[i] text.[i + m]
          else current_hash
        in
        search (i + 1) next_hash next_acc
    in
    search 0 initial_hash []

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* Function to generate all combinations of n-length strings from the alphabet, limited to max_iter *)
let nb_patterns = 100
let generate_combinations alphabet n =
  let rec aux prefix len acc =
    if List.length acc >= nb_patterns then
      acc
    else if len = 0 then
      (prefix :: acc)
    else
      List.fold_left (fun acc c ->
        aux (prefix ^ (String.make 1 c)) (len - 1) acc
      ) acc alphabet
  in
  aux "" n []

(* Function to compute the average of a list of integers *)
let average lst =
  let sum = List.fold_left (+) 0 lst in
  float_of_int sum /. float_of_int (List.length lst)

let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let text = read_file filename in
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" in
    for n = 2 to 10 do
      let combinations = generate_combinations (String.to_seq alphabet |> List.of_seq) n in
      let false_positive_counts = List.map (fun pattern ->
        false_positive_count := 0;
        ignore (rabin_karp_all text pattern);
        !false_positive_count
      ) combinations in
      let avg_false_positives = average false_positive_counts in
      Printf.printf "Average number of false positives for n=%d: %f\n" n avg_false_positives
    done

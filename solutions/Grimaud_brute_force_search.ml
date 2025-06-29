let brute_force_search text pattern =
  let n = String.length text in
  let m = String.length pattern in
  let rec search_at i =
    if i > n - m then
      -1
    else if String.sub text i m = pattern then
      i
    else
      search_at (i + 1)
  in
  search_at 0

let brute_force_search_all text pattern =
  let n = String.length text in
  let m = String.length pattern in
  let rec search_at i acc =
    if i > n - m then
      acc
    else if String.sub text i m = pattern then
      search_at (i+1) (acc@[i])
    else
      search_at (i+1) acc 
  in
  search_at 0 []

let () =
  if Array.length Sys.argv < 3 then
    Printf.printf "Usage: %s <text> <pattern>\n" Sys.argv.(0)
  else
    let text = Sys.argv.(1) in
    let pattern = Sys.argv.(2) in
    (*let text = "GCTAAGCAGGATGACTGTCTGACTGTCTGA" in
      let pattern = "CTGTCTGA" in*)
    let pos = brute_force_search text pattern in
    begin
      match pos with
      | -1 -> Printf.printf "Pattern \"%s\" not found.\n" pattern
      | _ -> Printf.printf "Pattern \"%s\" found at position %d.\n" pattern pos
    end;
    let pos_list = brute_force_search_all text pattern in
    begin
      match pos_list with
      | [] -> Printf.printf "Pattern \"%s\" not found.\n" pattern
      | _ -> Printf.printf "Pattern \"%s\" found at positions : " pattern;
        List.iter (fun pos -> Printf.printf "%d " pos) pos_list;
        print_newline()
    end
    

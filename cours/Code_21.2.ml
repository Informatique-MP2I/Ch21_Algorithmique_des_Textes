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

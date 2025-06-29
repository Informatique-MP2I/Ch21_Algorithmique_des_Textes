let boyer_moore_bad_char text pattern =
  let n = String.length text in
  let m = String.length pattern in
  let table = bad_char_shift pattern in 
  let rec search i =
    if i > n-m then (* pattern not found *)
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

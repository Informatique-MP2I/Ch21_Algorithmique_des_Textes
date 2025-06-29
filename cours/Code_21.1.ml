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

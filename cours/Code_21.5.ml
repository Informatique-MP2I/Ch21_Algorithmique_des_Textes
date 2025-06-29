let rabin_karp text pattern =
  let n = String.length text in
  let m = String.length pattern in
  if m > n then -1
  else
    let pattern_hash = hash pattern in
    let initial_hash = hash (String.sub text 0 m) in
    let rec search i current_hash =
      if i > n - m then -1
      else
      if current_hash=pattern_hash && (String.sub text i m)=pattern then i
      else
        let next_hash =
          if i < n - m then
            rolling_hash current_hash text.[i] text.[i + m]
          else current_hash
        in
        search (i + 1) next_hash
    in
    search 0 initial_hash

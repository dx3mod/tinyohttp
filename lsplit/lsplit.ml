let lsplit c str =
  try
    let split_index = String.index_from str 0 c in
    let str_len_after_split = String.length str - split_index - 1 in

    Some
      ( String.sub str 0 split_index,
        String.sub str (succ split_index) str_len_after_split )
  with Not_found -> None

(* Range library for making easy folding on a sequence of integers
Copyright (C) 2018 Aldrik KLEBER

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version. *)

  type t = {
    start : int;
    stop : int
  }

  let from start stop = { start = (min start stop) ; stop = (max start stop)}

  let fold f acc {start;stop} =
    let rec loop acc n =
      if n > stop then acc else
      loop (f acc n) (succ n) in
    loop acc start

  let split minimal n {start;stop} =
    let diff = (stop - start) in
    if (diff <= n) || (diff < minimal) then [from start stop] else
    let delta =  diff / n in
    let rec loop acc n =
      if n > stop then acc else
      let new_stop = n + delta in
      if new_stop > stop then (from n stop) :: acc else
      loop (from n new_stop :: acc) (succ new_stop) in
    loop [] start

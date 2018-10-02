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
let implode f r = f r.start r.stop

let from start stop = { start = (min start stop) ; stop = (max start stop)}

let fold f acc {start;stop} =
  let rec loop acc n =
    if n > stop then acc else
    loop (f acc n) (succ n) in
  loop acc start


let length = implode (fun start stop -> stop - start)

let split minimal n r =
  let range_big_enough minimal n size = (size <= n) || (size < minimal) in
  let diff = length r in
  if range_big_enough minimal n diff then [r] else
  let delta =  diff / n in
  let rec loop acc n =
    if n > r.stop then acc else
    let new_stop = n + delta in
    if new_stop > r.stop then (from n r.stop) :: acc else
    loop (from n new_stop :: acc) (succ new_stop) in
  loop [] r.start

let contain e r = r.start <= e || e <= r.stop

let cross a b = {start = (max a.start b.start) ; stop = (min a.stop b.stop)}

let join a b = {start = (min a.start b.start) ; stop = (max a.stop b.stop)}

let map f {start;stop} = {start=(f start);stop=(f stop)}

let aggregate f a b = {start = (f a.start b.start) ; stop = (f a.stop b.stop)}

(* Range library for making easy folding on a sequence of integers
Copyright (C) 2018 Aldrik KLEBER

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version. *)

type range_record = {start: int; stop: int}

type t =
  | Unfiltered of range_record
  | Filtered of range_record * ( int -> bool )

let implode f p =
  let r =
    match p with
    | Unfiltered r -> r
    | Filtered (r,_) -> r in
  f r.start r.stop

let from start stop = Unfiltered ({start= min start stop; stop= max start stop})

let filter_on f = function
  | Unfiltered r -> Filtered (r,f)
  | Filtered (r,f_prev_filter) ->
    let new_filter e = f_prev_filter e && f e in
    Filtered (r,new_filter)


let fold_by step f acc = function
  | Unfiltered {start; stop}->
    let rec loop acc n =
      if n > stop then acc
      else if n = stop then f acc n
      else loop (f acc n) (min stop (n + step))
    in
    loop acc start
  | Filtered ({start; stop},f_filter) ->
    let filter acc n = if f_filter n then f acc n else acc in
    let rec loop acc n =
      if n > stop then acc
      else if n = stop then filter acc n
      else loop (filter acc n) (min stop (n + step))
    in
    loop acc start

let rec fold_loop {start; stop} f acc n =
  if n > stop
    then acc
    else fold_loop {start; stop} f (f acc n) (succ n)

let fold f acc  = function
| Unfiltered r -> fold_loop r f acc r.start
| Filtered (r,f_filter) ->
  let f_agg acc n = if f_filter n then f acc n else acc in
  fold_loop r f_agg acc r.start

let rec iter_loop {start; stop} f n =
    if n > stop then ()
    else (
      f n ;
      iter_loop {start; stop} f (succ n) )

let iter f = function
| Unfiltered r -> iter_loop r f r.start
| Filtered (r,f_filter) ->
  let f_with_filter n = if f_filter n then f n else () in
  iter_loop r f_with_filter r.start

let length = implode (fun start stop -> stop - start)

let split minimal n r =
  let range_big_enough minimal n size = n >= minimal && size > minimal in
  let diff = length r in
  let packet_size =
    float_of_int diff /. float_of_int n |> ceil |> int_of_float
  in
  if range_big_enough minimal packet_size diff = false then [r]
  else
    let f acc n =
      match acc with
      | Some (next_start, result) -> Some (succ n, from next_start n :: result)
      | None -> Some (n, [])
    in
    r
    |> fold_by packet_size f None
    |> function None -> [] | Some (_, l) -> l

let contain e = function
| Unfiltered r ->
  r.start <= e && e <= r.stop
| Filtered (r, f_filter) ->
    f_filter e && r.start <= e && e <= r.stop

let get_range_record_from = function
| Filtered (r,_) -> r
| Unfiltered r -> r

let cross a b =
  let ra = get_range_record_from a in
  let rb = get_range_record_from b in
  from (max ra.start rb.start) (min ra.stop rb.stop)

let join a b =
  let ra = get_range_record_from a in
  let rb = get_range_record_from b in
  from (min ra.start rb.start) (max ra.stop rb.stop)

let map f  = function
  | Unfiltered {start; stop} -> Unfiltered {start= f start; stop= f stop}
  | Filtered ({start; stop},f_filter) ->
      from (f start) (f stop)
      |> filter_on f_filter

let aggregate f a b =
  let ra = get_range_record_from a in
  let rb = get_range_record_from b in
  from (f ra.start rb.start) (f ra.stop rb.stop)

let to_string = function
| Unfiltered r -> string_of_int r.start ^ ":" ^ string_of_int r.stop
| Filtered (r,_) -> "F:" ^ string_of_int r.start ^ ":" ^ string_of_int r.stop

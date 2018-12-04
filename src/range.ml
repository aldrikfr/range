(* Range library for making easy folding on a sequence of integers
Copyright (C) 2018 Aldrik KLEBER

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version. *)

type range_record = {start: int; stop: int}
    (*
type range_modification = Filter of (int -> bool)
*)
type t =
  | Natural of range_record
  | Modified of range_record * (int -> bool)

let no_common_area_msg = "There is no common area between the two ranges."
    (*
let apply_modifications f e =
  if f e then Some e else None
*)
let implode f p =
  let r = match p with Natural r -> r | Modified (r, _) -> r in
  f r.start r.stop

let from start stop = Natural {start= min start stop; stop= max start stop}

let filter_on f = function
  | Natural r -> Modified (r, f)
  | Modified (r, f_prev_filter) ->
      let new_filter e = f_prev_filter e && f e in
      Modified (r, new_filter)

let filtered_from start stop f_filter = from start stop |> filter_on f_filter

let is_filtered = function Natural _ -> false | Modified _ -> true

let remove_filter = function
  | Natural r -> Natural r
  | Modified (r, _) -> Natural r

let rec fold_by_loop {start; stop} step f acc n =
  if n > stop then acc
  else if n = stop then f acc n
  else fold_by_loop {start; stop} step f (f acc n) (min stop (n + step))

let fold_by step f acc = function
  | Natural r -> fold_by_loop r step f acc r.start
  | Modified (r, f_filter) ->
      let f_with_filter acc n = if f_filter n then f acc n else acc in
      fold_by_loop r step f_with_filter acc r.start

let rec fold_loop {start; stop} f acc n =
  if n > stop then acc else fold_loop {start; stop} f (f acc n) (succ n)

let fold f acc = function
  | Natural r -> fold_loop r f acc r.start
  | Modified (r, f_filter) ->
      let f_agg acc n = if f_filter n then f acc n else acc in
      fold_loop r f_agg acc r.start

let rec iter_loop {start; stop} f n =
  if n > stop then ()
  else (
    f n ;
    iter_loop {start; stop} f (succ n) )

let iter f = function
  | Natural r -> iter_loop r f r.start
  | Modified (r, f_filter) ->
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
    r |> fold_by packet_size f None |> function None -> [] | Some (_, l) -> l

let contain e = function
  | Natural r -> r.start <= e && e <= r.stop
  | Modified (r, f_filter) -> f_filter e && r.start <= e && e <= r.stop

let get_range_record_from = function Modified (r, _) -> r | Natural r -> r

let handle_result_with_exception = function
  | Ok r -> r
  | Error m -> failwith m

let cross a b =
  let ra = get_range_record_from a in
  let rb = get_range_record_from b in
  if ra.stop < rb.start || rb.stop < ra.start then Error no_common_area_msg
  else Ok (from (max ra.start rb.start) (min ra.stop rb.stop))

let cross_exn a b = cross a b |> handle_result_with_exception

let join a b =
  let ra = get_range_record_from a in
  let rb = get_range_record_from b in
  if ra.stop < rb.start || rb.stop < ra.start then Error no_common_area_msg
  else Ok (from (min ra.start rb.start) (max ra.stop rb.stop))

let join_exn a b = join a b |> handle_result_with_exception

let map f = function
  | Natural r -> Natural {start= f r.start; stop= f r.stop}
  | Modified (r, f_filter) -> from (f r.start) (f r.stop) |> filter_on f_filter

let aggregate f a b =
  let ra = get_range_record_from a in
  let rb = get_range_record_from b in
  from (f ra.start rb.start) (f ra.stop rb.stop)

let range_record_to_string r =
  string_of_int r.start ^ ":" ^ string_of_int r.stop

let to_string = function
  | Natural r -> range_record_to_string r
  | Modified (r, _) -> "F:" ^ range_record_to_string r

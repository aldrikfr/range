(* Range library for making easy folding on a sequence of integers
Copyright (C) 2018 Aldrik KLEBER

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version. *)

open Base

type range_record = {start: int; stop: int}

type t =
  | Natural of range_record
  | Modified of range_record * (int -> int option)

let no_common_area_msg = "There is no common area between the two ranges."

let implode f p =
  let r = match p with Natural r -> r | Modified (r, _) -> r in
  f r.start r.stop

let from start stop = Natural {start= min start stop; stop= max start stop}

let filter f = function
  | Natural r ->
      let modifier n = if f n then Some n else None in
      Modified (r, modifier)
  | Modified (r, f_prev) ->
      let modifier x =
        Option.(x |> f_prev >>= fun n -> if f n then Some n else None)
      in
      Modified (r, modifier)

let is_natural = function Natural _ -> true | Modified _ -> false

let reset = function Natural r -> Natural r | Modified (r, _) -> Natural r

let rec fold_by_loop {start; stop} step f acc n =
  if n > stop then acc
  else if n = stop then f acc n
  else fold_by_loop {start; stop} step f (f acc n) (min stop (n + step))

let fold_by step f acc = function
  | Natural r -> fold_by_loop r step f acc r.start
  | Modified (r, f_filter) ->
      let f_with_filter acc n =
        match f_filter n with Some mn -> f acc mn | None -> acc
      in
      fold_by_loop r step f_with_filter acc r.start

let rec fold_loop {start; stop} f acc n =
  if n > stop then acc else fold_loop {start; stop} f (f acc n) (Int.succ n)

let fold f acc = function
  | Natural r -> fold_loop r f acc r.start
  | Modified (r, f_filter) ->
      let f_agg acc n =
        match f_filter n with Some mn -> f acc mn | None -> acc
      in
      fold_loop r f_agg acc r.start

let rec iter_loop {start; stop} f n =
  if n > stop then ()
  else (
    f n ;
    iter_loop {start; stop} f (Int.succ n) )

let iter f = function
  | Natural r -> iter_loop r f r.start
  | Modified (r, f_filter) ->
      let f_with_filter n =
        match f_filter n with Some mn -> f mn | None -> ()
      in
      iter_loop r f_with_filter r.start

let length = implode (fun start stop -> stop - start)

let split minimal n r =
  let range_big_enough minimal n size = n >= minimal && size > minimal in
  let diff = length r in
  let packet_size =
    Float.of_int diff /. Float.of_int n |> Float.round_up |> Int.of_float
  in
  if not (range_big_enough minimal packet_size diff) then [r]
  else
    let f acc n =
      match acc with
      | Some (next_start, result) -> Some (Int.succ n, from next_start n :: result)
      | None -> Some (n, [])
    in
    r |> fold_by packet_size f None |> function None -> [] | Some (_, l) -> l

let contain e = function
  | Natural r -> r.start <= e && e <= r.stop
  | Modified _ as data ->
      fold (fun acc n -> if n = e then true else acc) false data

let get_range_record_from = function Modified (r, _) -> r | Natural r -> r

let handle_result_with_exception = function Ok r -> r | Error m -> failwith m

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
  | Natural r ->
      let modifier n = Some (f n) in
      Modified (r, modifier)
  | Modified (r, f_filter) ->
    let modifier mn = Option.(mn |> f_filter >>= (fun n -> Some (f n))) in
      Modified (r, modifier)

let range_record_to_string r =
  Int.to_string r.start ^ ":" ^ Int.to_string r.stop

let to_string = function
  | Natural r -> range_record_to_string r
  | Modified (r, _) -> "M:" ^ range_record_to_string r

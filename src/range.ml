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

let get_range_record_from = function Modified (r, _) -> r | Natural r -> r

let implode f p =
  let r = get_range_record_from p in
  f r.start r.stop

let from start stop = Natural {start= min start stop; stop= max start stop}

let filter f =
  let open Option in
  function
  | Natural r ->
      let modifier n = some_if (f n) n in
      Modified (r, modifier)
  | Modified (r, f_prev) ->
      let modifier = Fn.compose (filter ~f) f_prev in
      Modified (r, modifier)

let is_natural = function Natural _ -> true | Modified _ -> false

let reset r = Natural (get_range_record_from r)

let rec fold_by_loop {start; stop} step f acc n =
  if n > stop then acc
  else if n = stop then f acc n
  else fold_by_loop {start; stop} step f (f acc n) (min stop (n + step))

let fold_by step f acc = function
  | Natural r -> fold_by_loop r step f acc r.start
  | Modified (r, f_filter) ->
      let f_with_filter acc n =
        n |> f_filter |> Option.value_map ~default:acc ~f:(f acc)
      in
      fold_by_loop r step f_with_filter acc r.start

let rec fold_loop {start; stop} f acc n =
  if n > stop then acc else fold_loop {start; stop} f (f acc n) (Int.succ n)

let fold f acc = function
  | Natural r -> fold_loop r f acc r.start
  | Modified (r, f_filter) ->
      let f_agg acc n =
        n |> f_filter |> Option.value_map ~default:acc ~f:(f acc)
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
      let f_with_filter n = n |> f_filter |> Option.value_map ~default:() ~f in
      iter_loop r f_with_filter r.start

let length = implode (fun start stop -> stop - start)

let split minimal n r =
  let range_big_enough minimal n size = n >= minimal && size > minimal in
  let diff = length r in
  let packet_size =
    let open Float in
    of_int diff / of_int n |> round_up |> Int.of_float
  in
  if not (range_big_enough minimal packet_size diff) then [r]
  else
    let f acc n =
      match acc with
      | Some (next_start, result) ->
          Some (Int.succ n, from next_start n :: result)
      | None -> Some (n, [])
    in
    r |> fold_by packet_size f None |> Option.value_map ~default:[] ~f:snd

let contain e = function
  | Natural r -> r.start <= e && e <= r.stop
  | Modified _ as data -> fold (fun acc n -> n = e || acc) false data

let pair_map f (a, b) = (f a, f b)

let cross a b =
  let ra, rb = get_range_record_from (a, b) in
  if ra.stop < rb.start || rb.stop < ra.start then None
  else Some (from (max ra.start rb.start) (min ra.stop rb.stop))

let cross_exn a b = Option.value_exn ~message:no_common_area_msg (cross a b)

let join a b =
  let ra, rb = get_range_record_from (a, b) in
  if ra.stop < rb.start || rb.stop < ra.start then None
  else Some (from (min ra.start rb.start) (max ra.stop rb.stop))

let join_exn a b = Option.value_exn ~message:no_common_area_msg (join a b)

let map f = function
  | Natural r ->
      let modifier n = Some (f n) in
      Modified (r, modifier)
  | Modified (r, f_filter) ->
      let modifier n = Option.(n |> f_filter >>= fun n -> f n |> some) in
      Modified (r, modifier)

let range_record_to_string r =
  Int.(to_string r.start ^ ":" ^ to_string r.stop)

let to_string = function
  | Natural r -> "N:" ^ range_record_to_string r
  | Modified (r, _) -> "M:" ^ range_record_to_string r

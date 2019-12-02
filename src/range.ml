(* SPDX-License-Identifier: GPL-3.0-or-later *)
open Base

type modifiers = { 
    r : Limit.t ;
    f : (int -> int option)
}

type t = Natural of Limit.t | Modified of modifiers 

type elt = int

let no_common_area_msg = "There is no common area between the two ranges."

let get_limit_from = function Modified {r; _} -> r | Natural r -> r

let from start stop = Natural (Limit.from start stop)

let filter f = function
  | Natural r ->
      Modified {r;f=( fun n -> Option.some_if (f n) n)}
  | Modified m -> 
      Modified {r=m.r ; f=(Fn.compose (Option.filter ~f) m.f)}

let is_natural = function Natural _ -> true | Modified _ -> false

let reset r = Natural (get_limit_from r)

let fold_by step f acc = function
  | Natural r ->
      Limit.fold_by_loop r step f acc r.start
  | Modified {r; f=f_filter} ->
      let f_with_filter acc n =
        n |> f_filter |> Option.value_map ~default:acc ~f:(f acc)
      in
      Limit.fold_by_loop r step f_with_filter acc r.start


let fold_right f acc = function
  | Natural r ->
      Limit.fold_right_loop r f acc r.stop
  | Modified {r; f=f_filter} ->
      let f_agg acc n =
        n |> f_filter |> Option.value_map ~default:acc ~f:(f acc)
      in
      Limit.fold_right_loop r f_agg acc r.stop

let fold f acc = function
  | Natural r ->
      Limit.fold_loop r f acc r.start
  | Modified {r; f=f_filter} ->
      let f_agg acc n =
        n |> f_filter |> Option.value_map ~default:acc ~f:(f acc)
      in
      Limit.fold_loop r f_agg acc r.start

let to_list = fold (Fn.flip List.cons) []

let equal a b =
  match (a, b) with
  | Natural ra, Natural rb ->
      ra.start = rb.start && ra.stop = rb.stop
  | _  ->  List.equal Int.( = ) (to_list a) (to_list b)

let iter f = function
  | Natural r ->
      Limit.iter_loop r f r.start
  | Modified {r; f=f_filter} ->
      let f_with_filter n = n |> f_filter |> Option.value_map ~default:() ~f in
      Limit.iter_loop r f_with_filter r.start

let length p =
  match p with
    | Natural r -> r.stop - r.start
    | Modified _ ->  to_list p |> List.length

let split minimal n r =
  let big_enough minimal n size = n >= minimal && size > minimal in
  let diff = length r in
  let pack_size = Float.(of_int diff / of_int n |> round_up |> Int.of_float) in
  if not (big_enough minimal pack_size diff) then [r]
  else
    let f acc n =
      match acc with
      | Some (next_start, result) ->
          Some (Int.succ n, from next_start n :: result)
      | None ->
          Some (n, [])
    in
    r |> fold_by pack_size f None |> Option.value_map ~default:[] ~f:snd

let contain e = function
  | Natural r ->
      r.start <= e && e <= r.stop
  | Modified _ as data ->
      fold (fun acc n -> n = e || acc) false data

let pair_map f (a, b) = (f a, f b)

let agg_exn f a b = Option.value_exn ~message:no_common_area_msg (f a b)

let gen_agg flow fhigh a b =
  let ra, rb = pair_map get_limit_from (a, b) in
  if ra.stop < rb.start || rb.stop < ra.start then None
  else Some (from (flow ra.start rb.start) (fhigh ra.stop rb.stop))

let cross = gen_agg max min

let cross_exn = agg_exn cross

let join = gen_agg min max

let join_exn = agg_exn join

let map f = function
  | Natural r ->
      Modified {r; f=fun n -> Some (f n)}
  | Modified {r; f=f_filter} ->
      let new_f n = Option.(f_filter n >>= fun n -> f n |> some) in
      Modified {r; f=new_f}

let export_string r prefix = prefix ^ Limit.to_string r

let to_string = function
  | Natural r ->
      export_string r "Nat:"
  | Modified m ->
      export_string m.r "Mod:"

let of_string s =
  Option.value_exn ~message:"Unrecognized string format"
    (String.split ~on:':' s |> List.tl)
  |> List.map ~f:Int.of_string
  |> function [start; stop] -> from start stop | _ -> assert false

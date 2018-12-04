(* Range library for making easy folding on a sequence of integers
Copyright (C) 2018 Aldrik KLEBER

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version. *)

type t

val from : int -> int -> t

val filter_on : (int -> bool) -> t -> t

val filtered_from : int -> int -> (int -> bool) -> t

val remove_filter : t -> t

val is_filtered : t -> bool

val fold : ('a -> int -> 'a) -> 'a -> t -> 'a

val iter : (int -> unit) -> t -> unit

val split : int -> int -> t -> t list

val contain : int -> t -> bool

val cross : t -> t -> (t, string) result

val cross_exn : t -> t -> t

val join : t -> t -> (t, string) result

val join_exn : t -> t -> t

val map : (int -> int) -> t -> t

val aggregate : (int -> int -> int) -> t -> t -> t

val to_string : t -> string

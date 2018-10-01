(* Range library for making easy folding on a sequence of integers
Copyright (C) 2018 Aldrik KLEBER

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version. *)

type t

val from : int -> int -> t
val fold : ('a -> int -> 'a) -> 'a -> t -> 'a
val split : int -> int -> t -> t list

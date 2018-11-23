(* Range library for making easy folding on a sequence of integers
Copyright (C) 2018 Aldrik KLEBER

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version. *)

(** Range module provide a type for handling the description of an integer
sequence described by a start value and a stop value. This module provide
functions to fold this range, and some basic set operation likes cross and
join.
The main goal is to provide a split capacity in order to make life easy for
distributed processing. *)

(** t type correspond to a integer range value *)
type t

val from : int -> int -> t

val fold : ('a -> int -> 'a) -> 'a -> t -> 'a

val iter : (int -> unit) -> t -> unit

val split : int -> int -> t -> t list

val contain : int -> t -> bool

val cross : t -> t -> t

val join : t -> t -> t

val map : (int -> int) -> t -> t

val aggregate : (int -> int -> int) -> t -> t -> t

val to_string : t -> string

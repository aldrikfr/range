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
distributed processing.

@author Aldrik KLEBER

*)

(** t type correspond to a integer range value *)
type t

(** from int_value -> int_value will create a t value representing the range
described by the two values given in parameter.
Usage from 2 3 and from 3 2 are equivalent

@param start : int representating the starting value of the range
@param stop : int representing the last value of the range
@return range : t the corresponding range type value defined by start and stop
 *)
val from : int -> int -> t

(** fold f acc_init range_value result
fold all the integers part of range value aplying f to them and the accumulator.
The result is the last value of acc. The behaviour is equivalent to a list fold
function. *)
val fold : ('a -> int -> 'a) -> 'a -> t -> 'a

val iter : (int -> unit) -> t -> unit

val split : int -> int -> t -> t list

val contain : int -> t -> bool

val cross : t -> t -> t

val join : t -> t -> t

val map : (int -> int) -> t -> t

val aggregate : (int -> int -> int) -> t -> t -> t

val to_string : t -> string

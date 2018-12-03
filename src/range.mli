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

    @param start Integer representating the starting value of the range
    @param stop Integer representing the last value of the range
    @return Range.t type which value defined by start and stop parameters

 *)
val from : int -> int -> t

(** filter_on predicate function
    attach a predicate that will modify the behaviour of iter or fold funtions
    in order to apply only to values that satisfies the predicate.

    @param predicate the predicate is attached to the range value, the predicate
    must respect the signature int -> bool/
    @param Range.t value if the range provided has already a filter, the new
    range value will merge the two filters.
    @return new Range.t value with a new filter added.
    **)
val filter_on : (int -> bool) -> t -> t

(** fold the equivalent of List.fold_left applied to integer range_record
    explore all the values contained in the rang value applying f to the
    accumulator and the current element read by fold. If a filter was associated
    to the range value, only element validated by the predicate f will be passed
    to the function.

    @param f function aggregating the accumulator to the current value.
    @param acc initial value of the accumulator
    @param explored range value
    @return value of the accumulator after reading all elements **)
val fold : ('a -> int -> 'a) -> 'a -> t -> 'a

(** iter apply a function with side effect on all values of the range. This
    function support filtering.

    @param f function receiving an integer and returning unit
    @param range value
    @return unit
   **)
val iter : (int -> unit) -> t -> unit

val split : int -> int -> t -> t list

val contain : int -> t -> bool

val cross : t -> t -> t

val join : t -> t -> t

val map : (int -> int) -> t -> t

val aggregate : (int -> int -> int) -> t -> t -> t

val to_string : t -> string

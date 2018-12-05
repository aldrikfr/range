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

(** filter on predicate function
    attach a predicate that will modify the behaviour of iter or fold funtions
    in order to apply only to values that satisfies the predicate.

    @param predicate the predicate is attached to the range value, the predicate
    must respect the signature int -> bool
    @param Range.t value if the range provided has already a filter, the new
    range value will merge the two filters.
    @return new Range.t value with a new filter added.
    **)
val filter : (int -> bool) -> t -> t

(** create a new range of integer with a filters

    @param start Integer representating the starting value of the range
    @param stop Integer representing the last value of the range
    @param predicate the predicate is attached to the range value, the predicate
    must respect the signature int -> bool
    @return new Range.t type which value defined by start and stop parameters
    **)
val filtered_from : int -> int -> (int -> bool) -> t

(** remove all map and filter effects from a range.

    @param old Range.t value
    @return new Range.t value from parameter without modifiers.
   **)
val reset : t -> t

(** is filtered predicate

    test if a Range.t value contain a filter or not.
    @param Range.t value to test
    @return test true if there is a filter false otherwise
   **)
val is_filtered : t -> bool

(** fold the equivalent of List.fold_left applied to integer range_record
    explore all the values contained in the rang value applying f to the
    accumulator and the current element read by fold. If a filter was associated
    to the range value, only element validated by the predicate f will be passed
    to the function.

    @param f function aggregating the accumulator to the current value.
    @param acc initial value of the accumulator
    @param range explored range value
    @return value of the accumulator after reading all elements **)
val fold : ('a -> int -> 'a) -> 'a -> t -> 'a

(** iter apply a function with side effect on all values of the range. This
    function support filtering.

    @param f function receiving an integer and returning unit
    @param range value
    @return unit
   **)
val iter : (int -> unit) -> t -> unit

(** split a range value into a list of smaller range, useful for batching in
    parallel processing.

    @param minimal size of a range
    @param count number of subranges contained in the list.
    @param range value to split
    @return list of ranges with a size of minimal or greater, the list having
    count elements max.
    **)
val split : int -> int -> t -> t list

(** contain function to test if an integer value is contained in a Range.t values

    @param element to be tested
    @param reference range value
    @result true if element is contained in reference
    **)
val contain : int -> t -> bool

(** new Range.t value representing the common value between two Range.t values.

    @param a Range.t value
    @param b Range.t value
    @return Result type with a Range.t value defined by the common values, and
    Error string message if it is impossible to find common values.
    **)
val cross : t -> t -> (t, string) result

(** Same as cross function with exception for error handling.
   **)
val cross_exn : t -> t -> t

(** Join to generate a new Range.t value contained both a and b

    @param a Range.t value
    @param b Range.t value
    @return Result type with a Range.t value containing both a and b.
    If a and b are disjoint, they can't be joinded so an Error string message
    is returned.
   **)
val join : t -> t -> (t, string) result

(** Same as join with exception for error handling
   **)
val join_exn : t -> t -> t

(** apply f to limits value of Range.t value

    This feature is in DEVELOPMENT and is INSTABLE

    @param f function to apply to the limits of a range value
    @param r range to modify
    @return updated range
   **)
val map : (int -> int) -> t -> t


val aggregate : (int -> int -> int) -> t -> t -> t

val to_string : t -> string

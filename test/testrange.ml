(* SPDX-License-Identifier:  GPL-3.0-or-later *)
open OUnit2
open Base

let equality_tests =
[
  ("Issue #1 : A modified range can be equal to a natural one" >:: fun _ctxt ->
    let open Range in
    let data_a = from 3 6 in
    let data_b = from 2 5 |> map Int.succ in
    assert_bool "equality failed" (equal data_a data_b)
  )
]

let non_reg_tests = [
    equality_tests
] |> List.concat

let () =
 run_test_tt_main(
     "Range non regression bug testing" >::: non_reg_tests
 )

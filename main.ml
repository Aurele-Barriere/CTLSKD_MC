(* Main: testing the MC functions *)

open Common
open Models
open Logics
open Ltl_mc
open Ctls_mc
open Ctlskd_mc


(* LTL Example *)
let ltl_kripke: kripke  = [(1,[2;3]);(2,[]);(3,[])]
let ltl_marking1: marking = [(1,[2])]
let ltl_marking2: marking = [(1,[2;3])]
let ltl_init: state = 1
let ltl_spec: ltl = LTL_X (LTL_AP 1)

(* let _ = print_string (nusmv_write_pbm ltl_kripke ltl_init ltl_marking1 ltl_spec) *)

let bool1 = ltl_mc ltl_kripke ltl_init ltl_marking1 ltl_spec

let bool2 = ltl_mc ltl_kripke ltl_init ltl_marking2 ltl_spec

let _ = match bool1 with
  | true -> print_string "first test failed\n"
  | false -> print_string "first test passed\n"
let _ = match bool2 with
  | true -> print_string "second test passed\n"
  | false -> print_string "second test failed\n"
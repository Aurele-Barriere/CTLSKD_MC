(* Main: testing the MC functions *)

open Common
open Models
open Logics
open Ltl_mc
open Ctls_mc
open Ctlskd_mc


(* LTL Example *)
let kripke1: kripke  = [(1,[2;3]);(2,[]);(3,[])]
let marking1: marking = [(1,[2])]
let marking2: marking = [(1,[2;3])]
let init: state = 1
let ltl_spec: ltl = LTL_X (LTL_AP 1)

let ltl_1 = ltl_mc kripke1 init marking1 ltl_spec
let ltl_2 = ltl_mc kripke1 init marking2 ltl_spec

let ctl_spec1 = ST_CTLS_A (P_CTLS_X (P_CTLS_S (ST_CTLS_AP 1)))
let ctl_spec2 = ST_CTLS_E (P_CTLS_X (P_CTLS_S (ST_CTLS_AP 1)))

let ctls_1 = ctls_mc kripke1 init marking1 ctl_spec1
let ctls_2 = ctls_mc kripke1 init marking1 ctl_spec2

let _ = print_string "LTL Tests\n"
                   
let _ = match ltl_1 with
  | true -> print_string "first test failed\n"
  | false -> print_string "first test passed\n"
let _ = match ltl_2 with
  | true -> print_string "second test passed\n"
  | false -> print_string "second test failed\n"

let _ = print_string "CTLS Tests\n"
                   
let _ = match ctls_1 with
  | true -> print_string "first test failed\n"
  | false -> print_string "first test passed\n"
let _ = match ctls_2 with
  | true -> print_string "second test passed\n"
  | false -> print_string "second test failed\n"

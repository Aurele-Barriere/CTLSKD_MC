(* Main: testing the MC functions *)

open Common
open Models
open Logics
open Ltl_mc
open Ctls_mc
open Ctlskd_mc


let kripke1: kripke  = [(I 1,[I 2;I 3]);(I 2,[]);(I 3,[])]
let marking1: marking = [(1,[I 2])]
let marking2: marking = [(1,[I 2;I 3])]
let init: state = I 1
let ltl_spec: ltl = LTL_X (LTL_AP 1)

let ltl_1 = ltl_mc kripke1 init marking1 ltl_spec
let ltl_2 = ltl_mc kripke1 init marking2 ltl_spec

let ctl_spec1 = ST_CTLS_A (P_CTLS_X (P_CTLS_S (ST_CTLS_AP 1)))
let ctl_spec2 = ST_CTLS_E (P_CTLS_X (P_CTLS_S (ST_CTLS_AP 1)))

let ctls_1 = ctls_mc kripke1 init marking1 ctl_spec1
let ctls_2 = ctls_mc kripke1 init marking1 ctl_spec2

(* An observation mapping where o1 is blind and o2 perfect on two states *)
let obsm1 = ObsMap.empty
let obsm2 = ObsMap.add (1,1) 1 obsm1
let obsm3 = ObsMap.add (1,2) 1 obsm2
let obsm4 = ObsMap.add (2,1) 1 obsm3
let obsmap = ObsMap.add (2,2) 2 obsm4

let kripke2: std_kripke = [(1, [1;2]);(2, [1])]
let ctlskd_spec: history_ctlskd =
  H_CTLSKD_D (1,
              H_CTLSKD_A (
                  P_CTLSKD_X (
                      P_CTLSKD_H (
                          H_CTLSKD_K (
                              H_CTLSKD_AP 1)))))
let ctlskd_marking: std_marking = [(1, [1])]
let state_init: std_state = 2
let obs_init1 = 1
let obs_init2 = 2

let ctlskd_1 = ctlskd_mc kripke2 state_init ctlskd_marking obs_init1 obsmap ctlskd_spec
let ctlskd_2 = ctlskd_mc kripke2 state_init ctlskd_marking obs_init2 obsmap ctlskd_spec
                        
                     

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


let _ = print_string "CTLSKD Tests\n"
                   
let _ = match ctlskd_1 with
  | true -> print_string "first test failed\n"
  | false -> print_string "first test passed\n"
let _ = match ctlskd_2 with
  | true -> print_string "second test passed\n"
  | false -> print_string "second test failed\n"

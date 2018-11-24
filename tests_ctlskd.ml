open Common
open Models
open Logics
open Ltl_mc
open Ctls_mc
open Ctlskd_mc

let rec generate_spec_ctlskd (n:int): history_ctlskd =
  match n with
  | 0 -> H_CTLSKD_D (2, H_CTLSKD_K (H_CTLSKD_E (P_CTLSKD_U (P_CTLSKD_H H_CTLSKD_TRUE, P_CTLSKD_H (H_CTLSKD_AP 1)))))
  | _ -> H_CTLSKD_E (P_CTLSKD_X (P_CTLSKD_H (generate_spec_ctlskd (n-1))))
       
              
let rec generate_kripke_ctlskd (om:obs_marking) (n:int): (std_kripke * obs_marking) =
  match n with
  | 0 -> let o' = ObsMap.add (1, 0) 0 om in
         let newom = ObsMap.add (2, 0) 0 o' in
         ([(0,[0])], newom)
  | _ -> let (newk, newom) = generate_kripke_ctlskd om (n-1) in
         let odd = (2*n) - 1 in
         let even = 2*n in
         let o1 = ObsMap.add (1, odd) n newom in
         let o2 = ObsMap.add (1, even) n o1 in
         let o3 = ObsMap.add (2, odd) odd o2 in
         let o4 = ObsMap.add (2, even) even o3 in
         ((odd,[odd]) :: ((even, [even-2; odd]) :: newk), o4)
       
let main =
  let formula_size = int_of_string(Sys.argv.(1)) in
  let kripke_size = int_of_string(Sys.argv.(2)) in
  let _ = print_endline "CTLSKD Tests" in
  let (k,om) = generate_kripke_ctlskd (ObsMap.empty) kripke_size in
  let s = generate_spec_ctlskd formula_size in
  let init = (2 * kripke_size) in
  let m = [(1, [0])] in
  let obs_init = 1 in
  let result = ctlskd_mc k init m obs_init om s in
  match result with
  | true -> print_endline "test_passed"
  | false -> print_endline "test_failed"
                      

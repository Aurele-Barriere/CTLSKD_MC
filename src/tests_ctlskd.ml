open Common
open Models
open Logics
open Ltl_mc
open Ctls_mc
open Ctlskd_mc

let rec generate_spec_ctlskd (n:int): history_ctlskd =
  match n with
  | 0 -> H_CTLSKD_D (2, H_CTLSKD_K (H_CTLSKD_NEG (H_CTLSKD_AP 1)))
  | _ -> H_CTLSKD_E (P_CTLSKD_X (P_CTLSKD_H (generate_spec_ctlskd (n-1))))

let rec generate_kripke_ctlskd (om:obs_marking) (n:int): (std_kripke * obs_marking) =
  match n with
  | 0 -> failwith "model size should be at least 1"
  | 1 -> let o1 = ObsMap.add (1, 0) 0 om in
         let o2 = ObsMap.add (2, 0) 0 o1 in
         let o3 = ObsMap.add (1, 1) 0 o2 in
         let o4 = ObsMap.add (2, 1) 1 o3 in
         ([(1,[1]);(0,[0])], o4)
  | _ -> let (newk, newom) = generate_kripke_ctlskd om (n-1) in
         let o1 = ObsMap.add (1, n) 0 newom in (* blind *)
         let o2 = ObsMap.add (2, n) n o1 in    (* perfect *)
         ((n,[0;n-1])::newk,o2)     
                     
let main =
  let formula_size = int_of_string(Sys.argv.(1)) in
  let kripke_size = int_of_string(Sys.argv.(2)) in
  let _ = print_endline "CTLSKD Tests" in
  let (k,om) = generate_kripke_ctlskd (ObsMap.empty) kripke_size in
  let s = generate_spec_ctlskd formula_size in
  let init = kripke_size in
  let m = [(1, [0])] in
  let obs_init = 1 in
  let result = ctlskd_mc k init m obs_init om s in
  match result with
  | true -> print_endline "test_passed"
  | false -> print_endline "test_failed"
                      

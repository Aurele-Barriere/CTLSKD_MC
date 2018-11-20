(* Model Checking CTL*KD problems *)

open Common
open Models
open Logics
open Ctls_mc

(* Delta Update UD *)
let ud (s:std_state) (i:inf_set) (o:observation) (om:obs_marking): inf_set =
  List.filter (fun (x:std_state) -> eq_state om o s x) i

(* Temporal Update UT *)
let ut (s:std_state) (i:inf_set) (o:observation) (om:obs_marking): inf_set =
  []                            (* TODO *)

(* Initial information Set *)
let ii (s:std_state) (o:observation) (om:obs_marking) : inf_set =
  []                            (* TODO *)
                                                                 
       
       
(* CTL*KD model-Checking. Takes a model, an initial state, a marking and a specification *)
let ctlskd_mc (k:kripke) (state_init:state) (m:marking) (obs_init:observation) (om:obs_marking) (spec:history_ctlskd): bool = true

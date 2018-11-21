(* Model Checking CTL*KD problems *)

open Common
open Models
open Logics
open Ctls_mc

(* Given 2 lists witout duplicates, append them removing the duplicates *)
let rec append_no_dup (l1:'a list) (l2:' a list): 'a list =
  match l1 with
  | [] -> l2
  | x::l -> match List.mem x l2 with
            | true -> append_no_dup l l2
            | false -> x::(append_no_dup l l2)
       
(* Delta Update UD *)
let ud (s:std_state) (i:inf_set) (o:observation) (om:obs_marking): inf_set =
  List.filter (fun (x:std_state) -> eq_state om o s x) i

(* Temporal Update UT *)
let ut (s:std_state) (i:inf_set) (o:observation) (om:obs_marking) (states:std_state list): inf_set =             
  []                            (* TODO *)

(* Initial information Set *)
let ii (s:std_state) (o:observation) (om:obs_marking) (states:std_state list): inf_set =
  List.filter (fun (x:std_state) -> eq_state om o s x) states
                                                                 
       
       
(* CTL*KD model-Checking. Takes a model, an initial state, a marking and a specification *)
let ctlskd_mc (k:kripke) (state_init:state) (m:marking) (obs_init:observation) (om:obs_marking) (spec:history_ctlskd): bool = true

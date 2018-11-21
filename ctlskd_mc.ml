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

(* Successors of an information set *)
let rec build_successors (i:inf_set) (k:std_kripke): inf_set =
  match i with
  | [] -> []
  | x::l -> append_no_dup (successors x k) (build_successors l k)
       
(* Delta Update UD *)
let ud (s:std_state) (i:inf_set) (o:observation) (om:obs_marking): inf_set =
  List.filter (fun (x:std_state) -> eq_state om o s x) i
              
(* Temporal Update UT *)
let ut (s:std_state) (i:inf_set) (o:observation) (om:obs_marking) (k:std_kripke): inf_set =
  List.filter (fun (x:std_state) -> eq_state om o s x) (build_successors i k)

(* Initial information Set *)
let ii (s:std_state) (o:observation) (om:obs_marking) (states:std_state list): inf_set =
  List.filter (fun (x:std_state) -> eq_state om o s x) states

(* Return all possible information states that contain true_state *)
let rec augment_inf (true_state:std_state) (states: std_state list): inf_set list =
  match states with
  | [] -> []
  | s::l when s = true_state -> List.map (fun (ii:inf_set) -> s::ii) (augment_inf true_state l)
  | s::l -> let augmented_l = augment_inf true_state l in
            augmented_l @ List.map (fun (ii:inf_set) -> s::ii) augmented_l

(* Builds list of all state and associated inf_sets *)
let rec augment_state_inf (remaining: std_state list) (states: std_state list): (std_state * inf_set) list =
  match remaining with
  | [] -> []
  | s::l -> (List.map (fun (x:inf_set) -> (s,x)) (augment_inf s states)) @
              augment_state_inf l states
              
(* Builds the list of all augmented states *)
let rec augmented_states (states_inf: (std_state * inf_set) list) (obs: observation list): state list =
  match obs with
  | [] -> []
  | o::l ->
     List.map (fun ((x,y):std_state * inf_set) -> A (x,y,o)) states_inf @
       (augmented_states states_inf l)

(* Builds the list of (augmented) successors of an augmented state *)
let augmented_successors (st:state) (om:obs_marking) (k:std_kripke): state list =
  match st with
  | I _ -> failwith "augmented successors are defined on augmented states"
  | A (s,i,o) ->
     List.map (fun (x:std_state) -> A(x,ut x i o om k,o)) (successors s k)

(* Builds the augmented kripke model of a standard one *)
let augmented_kripke (k:std_kripke) (obs:observation list) (om:obs_marking): kripke =
  let std_states = get_states k in 
  let aug_state_inf = augment_state_inf std_states std_states in
  let aug_states = augmented_states aug_state_inf obs in
  List.map (fun (x:state) -> (x, augmented_successors x om k)) aug_states
       
(* CTL*KD model-Checking. Takes a model, an initial state, a marking and a specification *)
let ctlskd_mc (k:kripke) (state_init:state) (m:marking) (obs_init:observation) (om:obs_marking) (spec:history_ctlskd): bool = true

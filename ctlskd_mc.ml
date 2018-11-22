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

(* Builds the augmented marking given a list of augmented states *)
let rec augmented_marking (m:std_marking) (states: state list): marking =
  match m with
  | [] -> []
  | (a,l)::m' -> (a,List.filter
                      (fun (x:state) -> match x with
                                        | I _ -> false
                                        | A (s,i,o) -> List.mem s l)
                      states)::
                   augmented_marking m' states

(* Is a CTL*KD formula a CTL* formula? No K or D *)
let rec is_ctls (p:path_ctlskd): bool =
  match p with
  | P_CTLSKD_H h -> is_history_ctls h
  | P_CTLSKD_NEG p' -> is_ctls p'
  | P_CTLSKD_OR (p1,p2) -> is_ctls p1 && is_ctls p2
  | P_CTLSKD_AND (p1,p2) -> is_ctls p1 && is_ctls p2
  | P_CTLSKD_X p' -> is_ctls p'
  | P_CTLSKD_U (p1,p2) -> is_ctls p1 && is_ctls p2
and is_history_ctls (h:history_ctlskd) =
  match h with
  | H_CTLSKD_NEG h' -> is_history_ctls h'
  | H_CTLSKD_OR (h1,h2) -> is_history_ctls h1 && is_history_ctls h2
  | H_CTLSKD_AND (h1,h2) -> is_history_ctls h1 && is_history_ctls h2
  | H_CTLSKD_A p -> is_ctls p
  | H_CTLSKD_E p -> is_ctls p
  | H_CTLSKD_K h -> false
  | H_CTLSKD_D (o,h) -> false
  | H_CTLSKD_TRUE -> true
  | H_CTLSKD_AP a -> true

(* Converting a ctlskd formula without K or D to a ctls formula *)
let rec ctlskd_to_ctls (p:path_ctlskd): path_ctls =
  match p with
  | P_CTLSKD_H h -> P_CTLS_S (history_ctlskd_to_ctls h)
  | P_CTLSKD_NEG p' -> P_CTLS_NEG (ctlskd_to_ctls p')
  | P_CTLSKD_OR (p1,p2) -> P_CTLS_OR (ctlskd_to_ctls p1, ctlskd_to_ctls p2)
  | P_CTLSKD_AND (p1,p2) -> P_CTLS_AND (ctlskd_to_ctls p1, ctlskd_to_ctls p2)
  | P_CTLSKD_X p' -> P_CTLS_X (ctlskd_to_ctls p')
  | P_CTLSKD_U (p1,p2) -> P_CTLS_U (ctlskd_to_ctls p1, ctlskd_to_ctls p2)
and history_ctlskd_to_ctls (h:history_ctlskd): state_ctls =
  match h with
  | H_CTLSKD_TRUE -> ST_CTLS_TRUE
  | H_CTLSKD_AP a -> ST_CTLS_AP a
  | H_CTLSKD_NEG h' -> ST_CTLS_NEG (history_ctlskd_to_ctls h')
  | H_CTLSKD_OR (h1,h2) -> ST_CTLS_OR (history_ctlskd_to_ctls h1, history_ctlskd_to_ctls h2)
  | H_CTLSKD_AND (h1,h2) -> ST_CTLS_AND (history_ctlskd_to_ctls h1, history_ctlskd_to_ctls h2)
  | H_CTLSKD_A p -> ST_CTLS_A (ctlskd_to_ctls p)
  | H_CTLSKD_E p -> ST_CTLS_E (ctlskd_to_ctls p)
  | H_CTLSKD_K h' -> failwith "not a ctls formula"
  | H_CTLSKD_D (o,h) -> failwith "not a ctls formula"

(* Returns list of augmented states where a CTLS spec holds *)
let check_ctls (k:kripke) (m:marking) (spec:state_ctls):state list =
  List.filter
    (fun (x:state) -> ctls_mc k x m spec)
    (get_states k)
           
(* CTL*KD model-Checking. Takes a model, an initial state, a marking and a specification *)
let ctlskd_mc (k:kripke) (state_init:state) (m:marking) (obs_init:observation) (om:obs_marking) (spec:history_ctlskd): bool = true

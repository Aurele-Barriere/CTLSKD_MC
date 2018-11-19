(* Model Checking CTL* problems *)

open Common
open Models
open Logics
open Ltl_mc

(* Is a CTL* formula linear? No A or E *)
let rec is_linear_ctls (p:path_ctls): bool =
  match p with
  | P_CTLS_NEG p' -> is_linear_ctls p'
  | P_CTLS_OR (p1,p2) -> is_linear_ctls p1 && is_linear_ctls p2
  | P_CTLS_AND (p1,p2) -> is_linear_ctls p1 && is_linear_ctls p2
  | P_CTLS_X p' -> is_linear_ctls p'
  | P_CTLS_U (p1,p2) -> is_linear_ctls p1 && is_linear_ctls p2
  | P_CTLS_S s -> is_lin_state_ctls s
and is_lin_state_ctls (s:state_ctls): bool =
  match s with
  | ST_CTLS_A _ -> false
  | ST_CTLS_E _ -> false
  | ST_CTLS_NEG s' -> is_lin_state_ctls s'
  | ST_CTLS_OR (s1,s2) -> is_lin_state_ctls s1 && is_lin_state_ctls s2
  | ST_CTLS_AND (s1,s2) -> is_lin_state_ctls s1 && is_lin_state_ctls s2
  | _ -> true
       
(* Converts linear CTL* formulas to LTL. Syntax is unchanged *)
let rec linear_ctls_to_ltl (p:path_ctls): ltl =
  match p with
  | P_CTLS_NEG p' -> LTL_NEG (linear_ctls_to_ltl p')
  | P_CTLS_OR (p1,p2) -> LTL_OR (linear_ctls_to_ltl p1, linear_ctls_to_ltl p2)
  | P_CTLS_AND (p1,p2) -> LTL_AND (linear_ctls_to_ltl p1, linear_ctls_to_ltl p2)
  | P_CTLS_X p' -> LTL_X (linear_ctls_to_ltl p')
  | P_CTLS_U (p1,p2) -> LTL_U (linear_ctls_to_ltl p1, linear_ctls_to_ltl p2)
  | P_CTLS_S s -> lin_state_ctls_to_ltl s
and lin_state_ctls_to_ltl (s:state_ctls): ltl =
  match s with
  | ST_CTLS_TRUE -> LTL_TRUE
  | ST_CTLS_AP a -> LTL_AP a
  | ST_CTLS_NEG s' -> LTL_NEG (lin_state_ctls_to_ltl s')
  | ST_CTLS_OR (s1,s2) -> LTL_OR (lin_state_ctls_to_ltl s1, lin_state_ctls_to_ltl s2)
  | ST_CTLS_AND (s1,s2) -> LTL_AND (lin_state_ctls_to_ltl s1, lin_state_ctls_to_ltl s2)
  | _ -> failwith "not a linear CTL* formula"
       
(* CTL* model-Checking. Takes a model, an initial state, a marking and a specification *)
let ctls_mc (k:kripke) (init:state) (m:marking) (spec:state_ctls): bool =
  true

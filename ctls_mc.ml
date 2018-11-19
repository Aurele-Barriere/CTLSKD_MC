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

(* replace E phi with ! A ! phi in a CTL* formula. Returns an equivalent formula without E *)
let rec remove_E (p:path_ctls): path_ctls =
  match p with
  | P_CTLS_S s -> P_CTLS_S (state_remove_E s)
  | P_CTLS_NEG p' -> P_CTLS_NEG (remove_E p')
  | P_CTLS_OR (p1,p2) -> P_CTLS_OR (remove_E p1, remove_E p2)
  | P_CTLS_AND (p1,p2) -> P_CTLS_AND (remove_E p1, remove_E p2)
  | P_CTLS_X p' -> P_CTLS_X (remove_E p')
  | P_CTLS_U (p1,p2) -> P_CTLS_U (remove_E p1, remove_E p2)
and state_remove_E (s:state_ctls): state_ctls =
  match s with
  | ST_CTLS_NEG s' -> ST_CTLS_NEG (state_remove_E s')
  | ST_CTLS_OR (s1,s2) -> ST_CTLS_OR (state_remove_E s1, state_remove_E s2)
  | ST_CTLS_AND (s1,s2) -> ST_CTLS_AND (state_remove_E s1, state_remove_E s2)
  | ST_CTLS_A p -> ST_CTLS_A (remove_E p)
  | ST_CTLS_E p -> ST_CTLS_NEG ( ST_CTLS_A ( P_CTLS_NEG ( remove_E p)))
  | _ -> s

(* Returns list of states where a ltl spec holds *)
let check_ltl (k:kripke) (m:marking) (spec:ltl): state list =
  List.filter
    (fun (x:state) -> ltl_mc k x m spec)
    (get_states k)
           
(* The input shouldn't have E. Returns a new spec and a new marking. For each A phi formula where phi is ltl, A phi is replaced with a fresh variable and the marking updated. We keep doing that until the formula has no A operator left and is thus purely linear *)
let rec marking_update (k:kripke) (m:marking) (spec:state_ctls): (marking * state_ctls) =
  match spec with
  | ST_CTLS_TRUE -> (m,spec)
  | ST_CTLS_AP a -> (m,spec)
  | ST_CTLS_NEG s -> let (newm, newspec) = marking_update k m s in
                     (newm, ST_CTLS_NEG newspec)
  | ST_CTLS_OR (s1,s2) -> let (newm1, newspec1) = marking_update k m s1 in
                          let (newm2, newspec2) = marking_update k newm1 s2 in
                          (newm2, ST_CTLS_OR (newspec1, newspec2))
  | ST_CTLS_AND (s1,s2) -> let (newm1, newspec1) = marking_update k m s1 in
                          let (newm2, newspec2) = marking_update k newm1 s2 in
                          (newm2, ST_CTLS_AND (newspec1, newspec2))
  | ST_CTLS_A p ->
     begin match is_linear_ctls p with
     | true -> let fresh = fresh_atp m in
               let newm = (fresh, check_ltl k m (linear_ctls_to_ltl p))::m in
               (newm, ST_CTLS_AP fresh)
     | false -> let (newm, newspec) = path_marking_update k m p in
                marking_update k newm (ST_CTLS_A newspec)
     end
  | ST_CTLS_E _ -> failwith "This function only accepts specs without E"
and path_marking_update (k:kripke) (m:marking) (spec:path_ctls): (marking * path_ctls) =
  match spec with
  | P_CTLS_S s -> let (newm, newspec) = marking_update k m s in
                  (newm, P_CTLS_S newspec)
  | P_CTLS_NEG p -> let (newm, newspec) = path_marking_update k m p in
                    (newm, P_CTLS_NEG newspec)
  | P_CTLS_OR (p1,p2) -> let (newm1, newspec1) = path_marking_update k m p1 in
                         let (newm2, newspec2) = path_marking_update k newm1 p2 in
                         (newm2, P_CTLS_OR (newspec1, newspec2))
  | P_CTLS_AND (p1,p2) -> let (newm1, newspec1) = path_marking_update k m p1 in
                         let (newm2, newspec2) = path_marking_update k newm1 p2 in
                         (newm2, P_CTLS_AND (newspec1, newspec2))
  | P_CTLS_X p -> let (newm, newspec) = path_marking_update k m p in
                  (newm, P_CTLS_X newspec)
  | P_CTLS_U (p1,p2) -> let (newm1, newspec1) = path_marking_update k m p1 in
                         let (newm2, newspec2) = path_marking_update k newm1 p2 in
                         (newm2, P_CTLS_U (newspec1, newspec2))
    
       
(* CTL* model-Checking. Takes a model, an initial state, a marking and a specification *)
let ctls_mc (k:kripke) (init:state) (m:marking) (spec:state_ctls): bool =
  let simplspec = state_remove_E spec in (* simpl_spec has no E *)
  let (newm, newspec) = marking_update k m simplspec in (* newpsec is ltl *)
  ltl_mc k init newm (lin_state_ctls_to_ltl newspec)

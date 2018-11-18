(* Model Checking CTL* problems *)

open Common
open Models
open Logics
open Ltl_mc

(* CTL* model-Checking. Takes a model, an initial state, a marking and a specification *)
let ctls_mc (k:kripke) (init:state) (m:marking) (spec:state_ctls): bool =
  true

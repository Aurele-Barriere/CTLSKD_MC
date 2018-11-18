(* Model Checking CTL*KD problems *)

open Common
open Models
open Logics
open Ctls_mc

(* CTL*KD model-Checking. Takes a model, an initial state, a marking and a specification *)
let ctlskd_mc (k:kripke) (init:state) (m:marking) (spec:history_ctlskd): bool = true

(* TODO: change it to an augmented kripke structure (observations) *)

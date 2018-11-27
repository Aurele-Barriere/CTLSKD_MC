(* Logics: LTL, CTL* and CTLSKD *)          

open Common

(* LTL formulas *)
type ltl =
  | LTL_TRUE
  | LTL_AP of atp
  | LTL_NEG of ltl
  | LTL_OR of ltl* ltl
  | LTL_AND of ltl* ltl
  | LTL_X of ltl
  | LTL_U of ltl* ltl

(* CTL* formulas (state formulas and path formulas) *)
type state_ctls =
  | ST_CTLS_TRUE
  | ST_CTLS_AP of atp
  | ST_CTLS_NEG of state_ctls
  | ST_CTLS_OR of state_ctls * state_ctls
  | ST_CTLS_AND of state_ctls * state_ctls
  | ST_CTLS_A of path_ctls
  | ST_CTLS_E of path_ctls
 and path_ctls =
   | P_CTLS_S of state_ctls
   | P_CTLS_NEG of path_ctls
   | P_CTLS_OR of path_ctls * path_ctls
   | P_CTLS_AND of path_ctls * path_ctls
   | P_CTLS_X of path_ctls
   | P_CTLS_U of path_ctls * path_ctls

(* CTL*KD formulas (history formulas and path formulas) *)
type history_ctlskd =
  | H_CTLSKD_TRUE
  | H_CTLSKD_AP of atp
  | H_CTLSKD_NEG of history_ctlskd
  | H_CTLSKD_OR of history_ctlskd * history_ctlskd
  | H_CTLSKD_AND of history_ctlskd * history_ctlskd
  | H_CTLSKD_A of path_ctlskd
  | H_CTLSKD_E of path_ctlskd
  | H_CTLSKD_K of history_ctlskd
  | H_CTLSKD_D of observation * history_ctlskd
 and path_ctlskd =
   | P_CTLSKD_H of history_ctlskd
   | P_CTLSKD_NEG of path_ctlskd
   | P_CTLSKD_OR of path_ctlskd * path_ctlskd
   | P_CTLSKD_AND of path_ctlskd * path_ctlskd
   | P_CTLSKD_X of path_ctlskd
   | P_CTLSKD_U of path_ctlskd * path_ctlskd

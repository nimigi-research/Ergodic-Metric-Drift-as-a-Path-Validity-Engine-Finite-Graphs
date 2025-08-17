(*
ErgodicMetricDrift: finite kernels, drift, small sets, and a statement skeleton.
Uses Coq Reals. Proofs of Meyn–Tweedie-level results are left as Axioms for now.
*)
Require Import Reals List.
From mathcomp Require Import ssreflect.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Section FiniteKernel.

Variable n : nat. (* number of states *)
Definition vec := nat -> R.
Definition mat := nat -> nat -> R.

(* Row-stochastic kernel *)
Record kernel := {
  P : mat;
  row_stochastic : forall i, (sum_f_R0 (fun j => P i j) (n-1)) = 1%R /\
                            forall j, 0 <= P i j
}.

(* Total variation *)
Definition TV (mu nu : vec) : R :=
  (/2)%R * (sum_f_R0 (fun i => Rabs (mu i - nu i)) (n-1)).

(* Drift data *)
Record drift_data := {
  Vfun : vec; lam b : R;
  Vlb : forall i, 1 <= Vfun i;
  lam01 : 0 <= lam < 1
}.

(* Geometric drift condition *)
Definition has_drift (K : kernel) (d : drift_data) : Prop :=
  forall i, (sum_f_R0 (fun j => (P K i j) * (Vfun d j)) (n-1))
            <= (lam d) * (Vfun d i) + (b d).

(* Small set *)
Record small_set (K : kernel) := {
  C : list nat; m : nat; eps : R; nu : vec;
  eps_pos : 0 < eps;
  nu_prob : (sum_f_R0 (fun j => nu j) (n-1)) = 1%R /\ forall j, 0 <= nu j;
  minorization :
    forall i, In i C -> forall j, (Nat.iter m (fun M => fun a b => sum_f_R0 (fun k => M a k * P K k b) (n-1)) (P K)) i j
                                  >= eps * (nu j)
}.

Axiom drift_minorization_geometric :
  forall (K : kernel) (d : drift_data) (S : small_set K),
  exists (R rho : R), 0 < rho < 1 /\ True.

End FiniteKernel.
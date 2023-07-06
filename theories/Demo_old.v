From Knuth Require Import Loader.


(* 群 *)
Variable G : Set.

(* + *)
Variable f : G -> G -> G.

(* 単位元 *)
Variable e : G.

(* - *)
Variable i : G -> G.

Infix "+" := f (at level 50, left associativity).

(* 結合律 *)
Axiom assoc : forall a b c, a + b + c = a + (b + c).

(* 左単位元 *)
Axiom id_l : forall a, e + a = a.

(* 左逆元 *)
Axiom inv_l : forall a, i a + a = e.

Axiom force_admit: forall (a: Prop), a.

MyDefine a := 1.

MyPrint f.

Definition t0 : (forall (a b: G), a + b = b + a) := force_admit (forall (a b: G), a + b = b + a).

(*

Axiom a0 : (forall (a b: G), a + b = b + a).

Definition t1 := force_admit (forall (a b: G), a + b = b + a).
Definition t2 := (forall (a b: G), a + b = b + a).

MyDefine t3 := 3.

MyPrint t0.
MyPrint t1.
(* MyPrint t2. *)
MyPrint a0.
PP t3.

(* Complete id_l inv_l assoc. *)

(* test *)
PPConstr (forall (a: G), a = a).
PPConstr (forall (a b: G), a + b = b + a).
PPConstr (force_admit (forall (a b: G), a + b = b + a)).
PPConstr (force_admit (forall (a b: G), Coq.Init.Logic.eq (Knuth.Demo.add (a, b), Knuth.Demo.add (b, a)))).

PPConstr (force_admit (-e = e)).
PPConstr (force_admit (e = e)).
PPConstr (force_admit (forall (a b: G), a + i b = a + i b)).
Definition bod := (force_admit (forall (a b: G), a + i b = a + i b)).

MyDefBod body.

Print body.

Goal (forall (a b: G), i (a + b) = (i b + i a)).
  Proof.
    apply body.
    Qed.

*)

(* Compl id_l inv_l assoc. *)

Complete id_l inv_l assoc.

Create HintDb my_hints.

#[local] Hint Resolve th0 : my_hints.
#[local] Hint Resolve th1 : my_hints.
#[local] Hint Resolve th2 : my_hints.
#[local] Hint Resolve th3 : my_hints.
#[local] Hint Resolve th4 : my_hints.
#[local] Hint Resolve th5 : my_hints.
#[local] Hint Resolve th6 : my_hints.
#[local] Hint Resolve th7 : my_hints.
#[local] Hint Resolve th8 : my_hints.
#[local] Hint Resolve th9 : my_hints.

Goal (forall (x y : G), y = i x + (x + y)).
Proof.
  auto with my_hints.
  Qed.

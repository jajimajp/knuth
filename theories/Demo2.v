From Knuth Require Import Loader.

(* 集合 *)
Variable G : Set.

(* * *)
Variable f : G -> G -> G.
Infix "*" := f (at level 40, left associativity).

(* 定数 *)
Variable e : G.
Variable c1 : G.
Variable c2 : G.

(* 公理 *)
Axiom a0 : forall x : G, e * x = x.
Axiom a1 : forall x : G, x * e = x.
Axiom a2 : forall x y z : G, (x * y) * z = x * (y * z).
Axiom a3 : c1 * (c2 * c1) = c2 * (c1 * c2).
Axiom a4 : c1 * c1 = e.
Axiom a5 : c2 * c2 = e.


(* 任意の命題を強制的に成り立たせる *)
(* Complete内で使用している *)
Axiom force_admit: forall (a: Prop), a.

(* 完備化 *)
Complete a0 a1 a2 a3 a4 a5.

(* 完備化によって得られた規則からHint Databaseを作る *)
(* TODO: Complete内で実行する *)
Create HintDb my_hints.
#[local] Hint Rewrite -> th0 : my_hints.
#[local] Hint Rewrite -> th1 : my_hints.
#[local] Hint Rewrite -> th2 : my_hints.
#[local] Hint Rewrite -> th3 : my_hints.
#[local] Hint Rewrite -> th4 : my_hints.
#[local] Hint Rewrite -> th5 : my_hints.
#[local] Hint Rewrite -> th6 : my_hints.
#[local] Hint Rewrite -> th7 : my_hints.

Goal forall x : G, c1 * c2 * c1 * c2 * c1 * c2 = e.
  Proof.
    intros.
    autorewrite with my_hints.
    reflexivity.
  Qed.


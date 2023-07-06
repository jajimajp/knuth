From Knuth Require Import Loader.

(* 群 *)
Variable G : Set.

(* + *)
Variable f : G -> G -> G.
Infix "+" := f (at level 50, left associativity).

(* 単位元 *)
Variable e : G.
Variable c1 : G.
Variable c2 : G.

(* - *)
(* Variable i : G -> G. *)

Axiom a0 : forall x : G, e + x = x.
Axiom a1 : forall x : G, x + e = x.
Axiom a2 : forall x y z : G, (x + y) + z = x + (y + z).
Axiom a3 : c1 + (c2 + c1) = c2 + (c1 + c2).
Axiom a4 : c1 + c1 = e.
Axiom a5 : c2 + c2 = e.


(* 任意の命題を強制的に成り立たせる *)
(* Complete内で使用している *)
Axiom force_admit: forall (a: Prop), a.

(* 完備化 *)
Complete a0 a1 a2 a3 a4 a5.

Print th0.
Print th1.
Print th2.
Print th3.
Print th4.
Print th5.
Print th6.
Print th7.

(* 完備化によって得られた規則からHint Databaseを作る *)
(* TODO: Complete内で実行する *)
Create HintDb my_hints.
#[local] Hint Rewrite -> th0 : my_hints.
#[local] Hint Rewrite -> th1 : my_hints.

Goal forall x : G, c2 + (c2 + x) = x.
  Proof.
    intros.
    autorewrite with my_hints.
    reflexivity.
  Qed.



#[local] Hint Resolve th1 : my_hints.
#[local] Hint Resolve th2 : my_hints.
#[local] Hint Resolve th3 : my_hints.
#[local] Hint Resolve th4 : my_hints.
#[local] Hint Resolve th5 : my_hints.
#[local] Hint Resolve th6 : my_hints.
#[local] Hint Resolve th7 : my_hints.

(* 例題 *)
Goal (forall (x y : G), y = i x + (x + y)).
Proof.
  auto with my_hints.
  Qed.

Comp2 id_l inv_l assoc.
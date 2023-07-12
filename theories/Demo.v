From Knuth Require Import Loader.

(* 集合 *)
Variable G : Set.
(* + *)
Variable f : G -> G -> G.
Infix "+" := f (at level 50, left associativity).
(* 単位元 *)
Variable e : G.
(* - *)
Variable i : G -> G.

(**** 公理 ****)
(* 結合律 *)
Axiom assoc : forall a b c, a + b + c = a + (b + c).
(* 左単位元 *)
Axiom id_l : forall a, e + a = a.
(* 左逆元 *)
Axiom inv_l : forall a, i a + a = e.

(* 示したい等式 *)
Goal forall x y : G, i x + (x + y) = y.
  Proof.
    intros.
    rewrite <- assoc.
    rewrite inv_l.
    rewrite id_l.
    reflexivity.
  Qed.








(* 任意の命題を強制的に成り立たせる *)
(* Complete内で使用している *)
Axiom force_admit: forall (a: Prop), a.

(* 完備化 *)
Complete id_l inv_l assoc.

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
#[local] Hint Rewrite -> th8 : my_hints.
#[local] Hint Rewrite -> th9 : my_hints.

(* 示したい等式 *)
Goal forall x y : G, i x + (x + y) = y.
Proof.
  intros.
  autorewrite with my_hints.
  reflexivity.
Qed.


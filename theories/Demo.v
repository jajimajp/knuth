From Knuth Require Import Loader.

(* 群 *)
Variable G : Set.

(* + *)
Variable f : G -> G -> G.
Infix "+" := f (at level 50, left associativity).

(* 単位元 *)
Variable e : G.

(* - *)
Variable i : G -> G.

(* 結合律 *)
Axiom assoc : forall a b c, a + b + c = a + (b + c).

(* 左単位元 *)
Axiom id_l : forall a, e + a = a.

(* 左逆元 *)
Axiom inv_l : forall a, i a + a = e.

(* 任意の命題を強制的に成り立たせる *)
(* Complete内で使用している *)
Axiom force_admit: forall (a: Prop), a.

(* 完備化 *)
Complete id_l inv_l assoc.

(* 完備化によって得られた規則からHint Databaseを作る *)
(* TODO: Complete内で実行する *)
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

(* 例題 *)
Goal (forall (x y : G), y = i x + (x + y)).
Proof.
  auto with my_hints.
  Qed.

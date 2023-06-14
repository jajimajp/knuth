From Knuth Require Import Loader.

Theorem eq1: 1 = 1. reflexivity. Qed.
Theorem eq2: 2 = 2. reflexivity. Qed.
Theorem thm: forall (a: nat), a = a. Proof. auto. Qed.
Theorem assoc: forall (a b c: nat), (a + b) + c = a + (b + c).
  Admitted.

PP eq1.
PP eq2.
PP thm.
PP assoc.
PrintTheorem eq1.
(* 結果: 1 = 1 *)


PrintTheorem thm.
(* 結果: a = a *)

Set Printing All.

PrintTheorem assoc.
(* 結果: Nat.max 0 1 = 1 *)

Theorem max1 : max 0 1 = 1. Proof. auto. Qed.

(* (Nat.add (Nat.add (a, b), c)) -> Nat.add (a, Nat.add (b, c)) *)
PrintTheorem max1.
(* 結果: Nat.max 0 1 = 1 *)

Fixpoint append (l l' : list nat) :=
  match l with
  | nil => l'
  | cons h t => cons h (append t l')
  end.
                                
Theorem app_assoc : forall (a b c : list nat),
    append (append a b) c = append a (append b c).
Admitted.

PrintTheorem app_assoc.
(* 結果: append (append a b) c = append a (append b c) *)

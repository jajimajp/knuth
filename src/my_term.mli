(**
  * 完備化プラグイン内で用いるための書換規則の表現
  * 等号を関数としてではなく特別に扱うなどの特徴がある
  *)

(* 書換規則 *)
type t

(* 規則内の変数を返す *)
val variables : t -> string list

(* tomaで用いられるTRS表現の文字列との相互変換 *)
val from_trs : string -> t
val to_trs : t -> string

val to_constrexpr : t -> Constrexpr.constr_expr

val from_constr : Constr.t -> t

val pr: t -> Pp.t

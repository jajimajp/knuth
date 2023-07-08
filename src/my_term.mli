(**
  * 完備化プラグイン内で用いるための書換規則の表現
  * 等号を関数としてではなく特別に扱うなどの特徴がある
  *)

(* 書換規則 *)
type t

(* 定数名の集合 *)
type constants

(* 規則内の変数を返す *)
val variables : t -> string list

(* tomaで用いられるTRS表現の文字列との相互変換 *)
val from_trs : string -> t
val to_trs : t -> string

val to_constrexpr : t -> constants -> Constrexpr.constr_expr

val from_constr : Constr.t -> t

(* Constr.tのリストをtのリストに変換したものと、定数（束縛されていないId）を返す *)
val parse_constrs : Constr.t list -> t list * constants

val pr: t -> Pp.t

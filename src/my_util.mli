(** ユーティリティ *)

val chars_of_string : string -> char list
val string_of_chars : char list -> string

(* 集合を表すユーティリティ *)
module Set : sig
  type 'a t

  val empty : 'a t

  val to_list : 'a t -> 'a list
  val from_list : 'a list -> 'a t

  val insert : 'a t -> 'a -> 'a t
  val delete : 'a t -> 'a -> 'a t

  (* 和集合 *)
  val union : 'a t -> 'a t -> 'a t

  (* 第１要素から、第２要素に含まれないもののみを含む集合を返す *)
  val exclude : 'a t -> 'a t -> 'a t
end

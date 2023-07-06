val string_of_theorem2 : Names.GlobRef.t -> string
val string_of_theorems : Names.GlobRef.t list -> string
val string_of_theorem_all : Names.GlobRef.t -> string
val dbg : unit -> Pp.t
val compl : Names.GlobRef.t list -> Pp.t
val dec : Evd.evar_map -> EConstr.t -> Pp.t
val get_modpath_of : Names.GlobRef.t -> Names.ModPath.t
(* val string_of_theorem3 : Names.ModPath.t -> Names.GlobRef.t -> string *)
val string_of_constr : Constrexpr.constr_expr -> string
val declare :
    Names.Id.t -> Evd.evar_map -> EConstr.t -> Names.GlobRef.t

val decBod :
  Names.Id.t -> Evd.evar_map -> Names.GlobRef.t

val complete :
  Names.GlobRef.t list -> Pp.t

val complete2 :
  Names.GlobRef.t list -> Pp.t
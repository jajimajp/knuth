(* Definitionと同様に、定理を宣言する *)
val declare : Names.Id.t -> Evd.evar_map -> EConstr.t -> Names.GlobRef.t

(* 完備化を行う *)
val complete : Names.GlobRef.t list -> Pp.t
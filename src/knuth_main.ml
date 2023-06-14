open Myppconstr
open Pp

let get_constant_body gref =
  let open Names.GlobRef in
  match gref with
  | VarRef _ -> failwith "variables are not supported"
  | IndRef _ -> failwith "inductive types are not supported"
  | ConstructRef _ -> failwith "constructors are not supported"
  | ConstRef cst ->
    let cb = Global.lookup_constant cst in
    cb

let string_of_theorem gref =
  let cb = get_constant_body gref in
  let typ = cb.const_type in
  (* let univs = cb.const_universes in *)
  let uctx =
    UState.of_names
      (Printer.universe_binders_with_opt_names (Declareops.constant_polymorphic_context cb) None)
  in
  let env = Global.env () and sigma = Evd.from_ctx uctx in
  let ecstr = EConstr.of_constr typ in
  let ext = Constrextern.extern_type env sigma ecstr in
  let pp = pr_lconstr_expr env sigma ext in
  string_of_ppcmds pp

let print_theorem gref =
  str (string_of_theorem gref)

let print_constant sp udecl =
  let cb = Global.lookup_constant sp in
  (* let val_0 = Global.body_of_constant_body Library.indirect_accessor cb in *)
  let typ = cb.const_type in
  (* let univs = cb.const_universes in *)
  let uctx =
    UState.of_names
      (Printer.universe_binders_with_opt_names (Declareops.constant_polymorphic_context cb) udecl)
  in
  let env = Global.env () and sigma = Evd.from_ctx uctx in
  (* let pr_ltype = pr_ltype_env env sigma in *)
  let ecstr = EConstr.of_constr typ in
  let ext = Constrextern.extern_type env sigma ecstr in
  pr_lconstr_expr env sigma ext
  (* hov 0 *) (* (pr_ltype typ) *)
  
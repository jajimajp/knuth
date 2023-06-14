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

let dbg_string_of_kind_of_term t =
  let open Constr in
  match t with
  | Rel _ -> "Rel"
  | Var _ -> "Var"
  | Meta _ -> "Meta"
  | Evar _ -> "Evar"
  | Sort _ -> "Sort"
  | Cast _ -> "Cast"
  | Prod _ -> "Prod"
  | Lambda _ -> "Lambda"
  | LetIn _ -> "LetIn"
  | App _ -> "App"
  | Const _ -> "Const"
  | Ind _ -> "Ind"
  | Construct _ -> "Construct"
  | Case _ -> "Case"
  | Fix _ -> "Fix"
  | CoFix _ -> "CoFix"
  | Proj _ -> "Proj"
  | Int _ -> "Int"
  | Float _ -> "Float"
  | Array _ -> "Array"

(* 与えられた ind: Names.inductive が "=" であるか判定 *)
(* HACK: Inductiveの文字列表現がCoq.Init.Logic.eq であるかで判断する *)
let is_eq constr =
  match Constr.kind constr with
  | Constr.Ind (ind, _) ->
    let (mutind, i) = ind in
    let smut = Names.MutInd.to_string mutind in
      smut = "Coq.Init.Logic.eq"
  | _ -> false

let rec string_of_kind_of_term t =
  let open Constr in
  match t with
  | Rel i -> "x" ^ string_of_int i (* HACK: xにDe Bruijnインデクスをつけて変数名にしている TODO: 適切な変数名にする *) 
  | Prod (annot, t1, t2) -> 
    (* let s1 = string_of_constr t1 in (* "forall a, "に該当 TODO: TRSの変数名宣言に書き換える *) *)
    let s2 = string_of_constr t2 in (* 等式に対応 *)
      s2
  (* 関数適用 *)
  | App (f, args) ->
    if is_eq f then
      let s_arg1 = string_of_constr args.(1) in
      let s_arg2 = string_of_constr args.(2) in
        s_arg1 ^ " -> " ^ s_arg2 
    else
      let s_func = string_of_constr f in
      let s_args =
        let rec aux = function
        | [] -> ""
        | [cst] -> string_of_constr cst
        | h :: t -> string_of_constr h ^ ", " ^ aux t
        in aux (Array.to_list args) in
        s_func ^ "(" ^ s_args ^ ")"
  | Const (k, univs) ->
      Names.Constant.to_string k
  (* ↓ Inductive型の名前 VariantやInductiveで導入される *)
  | Ind (ind, univs) ->
    let (mutind, i) = ind in
    let smut = Names.MutInd.to_string mutind in
    "[Ind " ^ smut ^ "]"
  (* ↓ Variantで導入されるInductive型のコンストラクタ *)
  | Construct (cstrct, univs) ->
      let ind, i = cstrct in (* ind: コンストラクタ名, i: コンストラクタの何番目か *)
      let (mutind, _) = ind in
      let smut = Names.MutInd.to_string mutind in
      "[Cst " ^ smut ^ ".(" ^ string_of_int i ^ ")" ^ "]"
  | _ -> "<TODO: " ^ dbg_string_of_kind_of_term t ^ ">"

and string_of_constr cst = string_of_kind_of_term (Constr.kind cst)

let string_of_theorem2 gref =
  let cb = get_constant_body gref in
  let typ = cb.const_type in
  string_of_kind_of_term (Constr.kind typ)

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
  
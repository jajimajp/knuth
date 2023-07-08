open Constrexpr

let get_constant_body gref =
  let open Names.GlobRef in
  match gref with
  | VarRef _ -> failwith "variables are not supported"
  | IndRef _ -> failwith "inductive types are not supported"
  | ConstructRef _ -> failwith "constructors are not supported"
  | ConstRef cst ->
    let cb = Global.lookup_constant cst in
    cb

let declare name sigma body =
  let cinfo = Declare.CInfo.make ~name ~typ:None () in
  let info = Declare.Info .make ~poly:false () in
  Declare.declare_definition ~info ~cinfo ~opaque:false ~body sigma

let complete grefs =
  let axiom_types = List.map (fun gref ->
    let cb = get_constant_body gref in
    cb.const_type
  ) grefs in
  let (axioms, constants) = My_term.parse_constrs axiom_types in
  let res = Toma.complete_using_toma axioms in
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let declare_th name t =
    let body = My_term.to_constrexpr t constants in
    let (sigma, t) = Constrintern.interp_constr_evars env sigma body in
    let cinfo = Declare.CInfo.make ~name ~typ:None () in
    let info = Declare.Info .make ~poly:false () in
    Declare.declare_definition ~info ~cinfo ~opaque:false ~body:t sigma in
  let rec aux l index =
    match l with
    | [] -> None
    | h :: t -> (
      let name = Names.Id.of_string ("th" ^ string_of_int index) in
      ignore (declare_th name h);
      aux t (index + 1)
    ) in
  let _ = aux res 0 in
  Pp.str "Success."


let constr_constrexpr_r = function
| CRef _ -> "CRef"
| CFix _ -> "CFix"
| CCoFix _ -> "CCoFix"
| CProdN _ -> "CProdN"
| CApp _ -> "CApp"
| CEvar _ -> "CEvar"
| CSort _ -> "CSort"
| CNotation _ -> "CNotation"
| _ -> "<Not implemented>"

let string_of_constrexpr (e : Constrexpr.constr_expr) : string =
  let rec string_of_constrexpr_r = function
  | CRef (qid, opt) ->
    let opts = (match opt with None -> "None" | Some a -> "Some todo") in
    "CRef (" ^ (Libnames.string_of_qualid qid) ^ ", " ^ opts ^ ")"
  | CApp (c, l) ->
    let cs = string_of_constrexpr_r c.v in
    let ls = List.map (fun (i, _) -> string_of_constrexpr_r i.CAst.v) l in
    let ls = String.concat "," ls in
    "CApp (" ^ cs ^ ls ^ ")"
  | CProdN (l, c) ->
    let cs = string_of_constrexpr_r c.v in
    "CProdN (" ^ cs ^ ")"
  | CNotation (scp, not, sub) ->
    let (_, s) = not in
    "CNotation (" ^ s ^ ")"
  | e -> "<not implemented" ^ constr_constrexpr_r e ^ ">"
  in string_of_constrexpr_r (e.CAst.v)
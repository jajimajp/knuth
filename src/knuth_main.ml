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
  let axioms = List.map (fun gref ->
    let cb = get_constant_body gref in
    let typ = cb.const_type in
    My_term.from_constr typ
  ) grefs in
  let res = Toma.complete_using_toma axioms in
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let declare_th name t =
    let body = My_term.to_constrexpr t in
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
open Names
open Pp
open Constrexpr
open Str

let get_constant_body gref =
  let open Names.GlobRef in
  match gref with
  | VarRef _ -> failwith "variables are not supported"
  | IndRef _ -> failwith "inductive types are not supported"
  | ConstructRef _ -> failwith "constructors are not supported"
  | ConstRef cst ->
    let cb = Global.lookup_constant cst in
    cb

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
    let s_univ = Pp.string_of_ppcmds (Univ.Instance.pr (fun _ -> Pp.str "a") univs) in
    "[Ind " ^ s_univ ^ smut ^ "]"
  (* ↓ Variantで導入されるInductive型のコンストラクタ *)
  | Construct (cstrct, univs) ->
      let ind, i = cstrct in (* ind: コンストラクタ名, i: コンストラクタの何番目か *)
      let (mutind, _) = ind in
      let s_univ = Pp.string_of_ppcmds (Univ.Instance.pr (fun _ -> Pp.str "a") univs) in
      let smut = Names.MutInd.to_string mutind in
      s_univ ^ smut ^ ".(" ^ string_of_int i ^ ")"
  | _ -> "<TODO: " ^ dbg_string_of_kind_of_term t ^ ">"

and string_of_constr cst = string_of_kind_of_term (Constr.kind cst)

and string_of_kind_of_term_detail t =
  let open Constr in
  match t with
  | Rel i -> "y" ^ string_of_int i (* HACK: xにDe Bruijnインデクスをつけて変数名にしている TODO: 適切な変数名にする *) 
  | Prod (annot, t1, t2) -> 
    "[Prod" ^ (
    let s1 = string_of_constr_detail t1 in (* "forall a, "に該当 TODO: TRSの変数名宣言に書き換える *)
    let s2 = string_of_constr_detail t2 in (* 等式に対応 *)
    s1 ^ "&" ^ s2
    )
    ^ "]"
  (* 関数適用 *)
  | App (f, args) ->
    if is_eq f then
      let s_arg1 = string_of_constr_detail args.(1) in
      let s_arg2 = string_of_constr_detail args.(2) in
        s_arg1 ^ " = " ^ s_arg2 
    else
      let s_func = string_of_constr_detail f in
      let s_args =
        let rec aux = function
        | [] -> ""
        | [cst] -> string_of_constr_detail cst
        | h :: t -> string_of_constr_detail h ^ ", " ^ aux t
        in aux (Array.to_list args) in
        "[App" ^ s_func ^ "(" ^ s_args ^ ")" ^ "]"
  | Const (k, univs) ->
      "[(Const)" ^
      (
        let user = Names.Constant.user k in
        let modpath = Names.KerName.modpath user in
        let knlabel = Names.KerName.label user in
        (Names.ModPath.debug_to_string modpath) ^ "<.>" ^ (Names.Id.to_string (Names.Label.to_id knlabel))
      )
      (* Names.Constant.to_string k *)
      ^ "]"
  (* ↓ Inductive型の名前 VariantやInductiveで導入される *)
  | Ind (ind, univs) ->
    let (mutind, i) = ind in
    let smut = Names.MutInd.to_string mutind in
    let s_univ = Pp.string_of_ppcmds (Univ.Instance.pr (fun _ -> Pp.str "a") univs) in
    "[Ind " ^ s_univ ^ smut ^ "]"
  (* ↓ Variantで導入されるInductive型のコンストラクタ *)
  | Construct (cstrct, univs) ->
      let ind, i = cstrct in (* ind: コンストラクタ名, i: コンストラクタの何番目か *)
      let (mutind, _) = ind in
      let s_univ = Pp.string_of_ppcmds (Univ.Instance.pr (fun _ -> Pp.str "a") univs) in
      let smut = Names.MutInd.to_string mutind in
      s_univ ^ smut ^ ".(" ^ string_of_int i ^ ")"
  | _ -> "<TODO: " ^ dbg_string_of_kind_of_term t ^ ">"

and string_of_constr_detail cst = string_of_kind_of_term_detail (Constr.kind cst)

(* HACK: 与えられた項のforall a: G のGのmodpathをとる *)
and get_modpath t =
  let open Constr in
  match t with 
  | Prod (_, t1, _) -> get_modpath (Constr.kind t1)
  | Const (k, _) ->
    let user = Names.Constant.user k in
    Names.KerName.modpath user
  | _ -> failwith "Invalid term"

let rec num_var constr =
  let open Constr in
  match constr with
  | Prod (_, _, c) -> 1 + num_var (Constr.kind c)
  |_  -> 0

(* 例: 3 -> "x1 x2 x3" *)
let vars_of_num num =
  let rec loop acc n =
    if n = num then loop ("x" ^ string_of_int n) (n - 1)
    else if n > 0 then
      loop ("x" ^ string_of_int n ^ " " ^ acc) (n - 1) 
    else acc in
  loop "" num
    
let string_of_theorem2 gref =
  let cb = get_constant_body gref in
  let typ = cb.const_type in
  string_of_kind_of_term (Constr.kind typ)

let string_of_theorem_all gref =
  let open Declarations in
  let cb = get_constant_body gref in
  let typ = cb.const_type in
  let bod = cb.const_body in
  let modpath = get_modpath (Constr.kind typ) in
  let modpaths = Names.ModPath.to_string modpath in
  modpaths ^
  string_of_kind_of_term_detail (Constr.kind typ) ^
  match bod with
  | Def _ -> "def" 
  | Undef inline -> (
    match inline with
    | Some i -> "inline: " ^ string_of_int i
    | None -> "inline with None"
  )
  | OpaqueDef _ -> "opaque"
  | Primitive _ -> "primitive"

let rec string_of_theorems = function
| [] -> ""
| hd :: tl -> string_of_theorem2 hd ^ "\n" ^ (string_of_theorems tl)

let kind_of_gref gref =
  let cb = get_constant_body gref in
  let typ = cb.const_type in
  Constr.kind typ

let toma_input theorems =
  let rec max_var_num acc ls =
    match ls with
    | [] -> acc
    | h::t ->
      let h = num_var (kind_of_gref h) in
      max_var_num (if acc < h then h else acc) t in
  let var_num = max_var_num 0 theorems in
  let vars_str = vars_of_num var_num in
  let theorems_str = string_of_theorems theorems in
  "(VAR " ^ vars_str ^ ")\n" ^
  "(RULES\n" ^ theorems_str ^ ")\n"

let execute_command cmd =
  let ic = Unix.open_process_in cmd in
  let rec loop acc read =
    try
      let line = input_line ic in
      if read then
        if line = "" then loop acc false
        else loop (acc ^ line ^ "\n") read
      else
        if line = "ES:" then loop acc true else loop acc false
    with
    | End_of_file -> acc
  in
  let result = loop "" false in
  ignore (Unix.close_process_in ic);
  result

let mktemp () =
  let ic = Unix.open_process_in "mktemp" in
  let rec loop acc =
    try
      let line = input_line ic in loop line
    with
    | End_of_file -> acc
  in
  let result = loop "" in
  ignore (Unix.close_process_in ic);
  result

let write f s =
  let oc = open_out f in
  Printf.fprintf oc "%s" s;
  close_out oc

type myterm =
(* | MyProd of string * myterm *)
| MyApp of string * myterm list
| MyVar of string

let rec string_of_term = function
  (* | MyProd (s, t) -> "forall " ^ s ^ ", " ^ string_of_term t *)
  | MyApp (s, ts) -> s ^ "( " ^ String.concat ", " (List.map string_of_term ts) ^ " )"
  | MyVar s -> s

let string_of_eq = function
| (t1, t2) -> (string_of_term t1 ^ " -> " ^ string_of_term t2)

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

(* トップレベルのカンマで区切る *)
let split_by_comma s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  let l = exp (String.length s - 1) [] in
  let rec loop acc l par =
    match l with
    | [] -> [acc]
    | ',' :: ' ' :: t -> if (par = 0) then acc :: (loop [] t par) else
      loop (acc @ [','; ' ']) t par
    | ',' :: t -> if (par = 0) then acc :: (loop [] t par) else
      loop (acc @ [','; ' ']) t par
    | '(' :: t -> loop (acc @ ['(']) t (par+1)
    | ')' :: t -> loop (acc @ [')']) t (par-1)
    | h :: t -> loop (acc @ [h]) t par in
  List.map string_of_chars (loop [] l 0)

let rec parse_term str =
  let reg = regexp "^\\([^(]+\\)(\\(.+\\))$" in
  let matched = string_match reg str 0 in
  if matched then
    let f = matched_group 1 str in
    let args = matched_group 2 str in
    (* let args = Str.split (Str.regexp ", ") args in *)
    let args = split_by_comma args in
    let args = List.map (fun s -> parse_term s) args in
    MyApp (f, args)
  else
    MyVar str

let parse_eq line =
  (* 長さは２でなければならない *)
  let terms = Str.split (Str.regexp " -> ") line in
  let t1 = parse_term (List.nth terms 0) in
  let t2 = parse_term (List.nth terms 1) in
  string_of_eq (t1, t2)

let eq_of_string s =
  let terms = Str.split (Str.regexp " -> ") s in
  let t1 = parse_term (List.nth terms 0) in
  let t2 = parse_term (List.nth terms 1) in
  (t1, t2)

let dbg () =
  str (parse_eq "Knuth.Demo.f(Knuth.Demo.e, x1) -> x1")

let compl grefs =
  let cwd = Sys.getcwd() in
  let tmpfile = mktemp () in
  let _ = write tmpfile (toma_input grefs) in
  let cmd = cwd ^ "/../../toma/toma --ordered-completion " ^ tmpfile ^ " --v" in
  let res = execute_command cmd in
  let res = String.concat "\n" (List.map (fun l -> parse_eq l) (split (regexp "\n") res)) in
  str res


let dec sigma t =
  str (string_of_kind_of_term (Constr.kind (EConstr.to_constr sigma t)))

let get_modpath_of gref =
  let cb = get_constant_body gref in
  let typ = cb.const_type in
  get_modpath (Constr.kind typ)
(*
let string_of_theorem3 modpath t =
  let open Constr in
  Prod (Context.make_annot Names.Name.Anonymous Irrelevant, 
    Const (Names.Constant.make1 (Names.KerName.make modpath (Names.Label.of_id (Names.Id.of_string "G"))), Univ.Instance.empty),
    Prod (Context.make_annot Names.Name.Anonymous Irrelevant, 
      Const (Names.Constant.make1 (Names.KerName.make modpath (Names.Label.of_id (Names.Id.of_string "G"))), Univ.Instance.empty),
      App (
        Const (Names.Constant.make1 (Names.KerName.make modpath (Names.Label.of_id (Names.Id.of_string "f"))), Univ.Instance.empty),
        Array.of_list([
          Rel 0
        ])
      )
    )
  )
*)

(*
let string_of_constrexpr_dbg c =
  let open Constrexpr in
  match c with
  | CRef _ -> "CRef"
  | CFix     _ -> "CFix     "
  | CCoFix   _ -> "CCoFix"
  | CProdN   _ -> "CProdN   "
  | CLambdaN _ -> "CLambdaN "
  | CLetIn   _ -> "CLetIn   "
  | CAppExpl _ -> "CAppExpl "
  | CApp     _ -> "CApp     "
  | CProj    _ -> "CProj    "
  | CRecord  _ -> "CRecord  "

  (* representation of the "let" and "match" constructs *)
  | CCases _ -> "CCases "
  | CLetTuple _ -> "CLetTuple "
  | CIf _ -> "CIf "
  | CHole   _ -> "CHole   "
  | CPatVar _ -> "CPatVar "
  | CEvar   _ -> "CEvar   "
  | CSort   _ -> "CSort   "
  | CCast   _ -> "CCast   "
  | CNotation _ -> "CNotation "
  | CGeneralization _ -> "CGeneralization "
  | CPrim _ -> "CPrim "
  | CDelimiters _ -> "CDelimiters "
  | CArray _ -> "CArray "

let string_of_lname = function
| Anonymous -> "anonymous" 
| Name ident -> Id.to_string ident

let string_of_binder_kind = function
| Default bk ->
  "Default (" ^
  (match bk with
  | Explicit -> "Explicit"
  | MaxImplicit -> "MaxInplicit"
  | NonMaxImplicit -> "NonMaxInplicit") ^ ")"
| Generalized _ -> "(generalized)"

let rec string_of_local_binder_expr = function
  (* l: a など *)
  | CLocalAssum (l, bk, c) ->
    let rec aux = (function
    | [] -> ""
    | h::t -> (CAst.map string_of_lname h).v ^ ";" ^ aux t) in
    let ls = "[" ^ aux l ^ "]" in
    let cs = (CAst.map string_of_constrexpr c).v in
    "CLocalAssum (" ^ ls ^ ", " ^ string_of_binder_kind bk ^ ", " ^ cs ^ ")"
  | CLocalDef _ -> "CLocalDef"
  | CLocalPattern _ -> "CLocalPattern"

and string_of_constr_notation_substitution s =
    match s with
    | [], [], [], [] -> "all[]"
    | l, [], [], [] ->
      let rec aux = (function
      | [] -> ""
      | h::t -> string_of_constrexpr h.CAst.v ^ ";" ^ aux t) in
      "[" ^ aux l ^ "]"
    | [], _, [], [] -> "snd"
    | [], [], _, [] -> "trd"
    | _ -> "else"

and string_of_constrexpr c =
  let open Constrexpr in
  match c with
  | CRef (qid, exp) ->
    let qs = Libnames.string_of_qualid qid in
    let exps = (match exp with
    | None -> "None"
    | Some exp -> "Some <TODO instance_expr>") in
    "CRef (" ^ qs ^ ", " ^ exps ^ ")"
  | CProdN (l, c0) ->
    let rec aux = (function
      | [] -> ""
      | h::t -> string_of_local_binder_expr h ^ aux t) in
    let ls = "[" ^ aux l ^ "]" in
    let c0s = (CAst.map string_of_constrexpr c0).v in
    "CProdN (" ^ ls ^ ", " ^ c0s ^ ")"
  | CApp (c0, l) ->
    let c0s = string_of_constrexpr c0.v in
    let s_aux (c, exp) =
      let cs = string_of_constrexpr c.CAst.v in
      let exps = (match exp with
      | None -> "None"
      | Some _ -> "Some TODO") in
      cs ^ "*" ^ exps in
    let rec aux = (function
    | [] -> ""
    | h::t -> s_aux h ^ ";" ^ aux t) in
    let ls = "[" ^ aux l ^ "]" in
    "CApp (" ^ c0s ^ ", " ^ ls ^ ")"
  | CNotation (with_scope, not, sub) ->
    let ss = (match with_scope with
    | None -> "None"
    | Some LastLonelyNotation -> "Some LastLonelyNotation"
    | Some (NotationInScope s) -> "Some NotationInScope" ^ s) in
    let ns = (match not with
    | (InConstrEntry, s) -> "(InConstrEntry, " ^ s ^ ")"
    | (InCustomEntry s1, s2) -> "(InCostomEntry(" ^ s1 ^ "), " ^ s2 ^ ")") in
    "CNotation (" ^ ss ^ ", " ^ ns ^ ", " ^ string_of_constr_notation_substitution sub ^ ")"
  | _ -> "<TODO " ^ string_of_constrexpr_dbg c ^ ">"
  
  *)
(*
let myeq: myterm * myterm =
  (
    MyApp (
      "+",
      [
        MyVar "a";
        MyApp (
          "-",
          [
            MyVar "b";
          ]
        )
      ]
    ),
    MyApp (
      "+",
      [
        MyVar "a";
        MyApp (
          "-",
          [
            MyVar "b";
          ]
        )
      ]
    )
  )
*)

let rec convert_term = function
| MyApp (f, l) ->
  if f = "+" || f = "Knuth.Demo2.f" then 
    let terms = List.map convert_term l in
    CAst.make (
      CNotation (
        None,
        (InConstrEntry, "_ + _"),
        (
          terms,
          [], [], []
        )
      )
    );
  else if f = "-" || f = "Knuth.Demo2.i" then
    (match List.map convert_term l with
    | [term] ->  
      CAst.make (
        CApp (
          CAst.make (CRef (Libnames.qualid_of_string "i", None)),
          [(term, None)]
        )
      );
    | _ -> failwith "Invalid term"
    )
  else failwith ("Not implemented" ^ f)
| MyVar x ->
  CAst.make (CRef (Libnames.qualid_of_string x, None))

let rec insert l x = match l with
| [] -> [x]
| h :: t -> if h = x then l else h :: insert t x

let rec union a b = match (a, b) with
| [], _ -> b
| _, [] -> a
| _, h :: t -> union (insert a h) t

let rec delete l x = match l with
| [] -> []
| h :: t -> if h = x then t else h :: delete t x

let rec exclude l a = match a with
| [] -> l
| h :: t -> exclude (delete l h) t

let vars (t1, t2) = 
  let rec vars_term = function
  | MyApp (f, args) ->
      let vs = List.map vars_term args in
      let vs = List.fold_left union [] vs in
      vs
  | MyVar x -> [x] in
  let vars = union (vars_term t1) (vars_term t2) in
  let globals = ["Knuth.Demo2.e"; "Knuth.Demo2.c1"; "Knuth.Demo2.c2"] in
  exclude vars globals

let convert (t1, t2) =
  let vars = vars (t1, t2) in
  let t1 = convert_term t1 in
  let t2 = convert_term t2 in
  CApp (
    CAst.make (
      CRef (
        Libnames.qualid_of_string "force_admit",
        None
      )
    ),
    [
      (
        CAst.make (
          CProdN (
            [
              CLocalAssum (
                List.map (fun name -> 
                  CAst.make (Names.Name.mk_name (Id.of_string name))
                ) vars,
                (* [
                  CAst.make (Names.Name.mk_name (Id.of_string "a"));
                  CAst.make (Names.Name.mk_name (Id.of_string "b"));
                  CAst.make (Names.Name.mk_name (Id.of_string "c"));
                ], *)
                Default Explicit,
                CAst.make (CRef (Libnames.qualid_of_string "G", None))
              )
            ],
            CAst.make (
              CNotation (
                None,
                (InConstrEntry, "_ = _"),
                (
                  [
                    t1;
                    t2;
                  ], [],[],[]
                )
              )
            )
          )
        )
      ), None
    ]
  )

let string_of_constr c =
  (* let c = CAst.make (convert myeq) in *)
  let myeq = eq_of_string "Knuth.Demo.i(Knuth.Demo.f(x132030, x13202)) -> Knuth.Demo.f(Knuth.Demo.i(x13202), Knuth.Demo.i(x132030))" in 
  (* let mybody = CAst.make (convert myeq) in
  let _ = CAst.map string_of_constrexpr mybody in *)
  string_of_eq myeq 
  (* ast.v *)

let declare name sigma body =
  let cinfo = Declare.CInfo.make ~name ~typ:None () in
  let info = Declare.Info .make ~poly:false () in
  Declare.declare_definition ~info ~cinfo ~opaque:false ~body sigma

let decBod name sigma =
  let env = Global.env () in
  let cinfo = Declare.CInfo.make ~name ~typ:None () in
  let info = Declare.Info .make ~poly:false () in
  let myeq = eq_of_string "Knuth.Demo.i(Knuth.Demo.f(x132030, x13202)) -> Knuth.Demo.f(Knuth.Demo.i(x13202), Knuth.Demo.i(x132030))" in 
  let mybody = convert myeq in
  let (sigma, t) = Constrintern.interp_constr_evars env sigma (CAst.make mybody) in
  Declare.declare_definition ~info ~cinfo ~opaque:false ~body:t sigma

let complete grefs =
  let cwd = Sys.getcwd() in
  let tmpfile = mktemp () in
  let _ = write tmpfile (toma_input grefs) in
  let cmd = cwd ^ "/../../toma/toma --ordered-completion " ^ tmpfile ^ " --v" in
  let res = execute_command cmd in
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let eqs = List.map (fun s -> eq_of_string s) (split (regexp "\n") res) in
  let declare_eq name eq =
    let body = CAst.make (convert eq) in
    let (sigma, t) = Constrintern.interp_constr_evars env sigma body in
    let cinfo = Declare.CInfo.make ~name ~typ:None () in
    let info = Declare.Info .make ~poly:false () in
    Declare.declare_definition ~info ~cinfo ~opaque:false ~body:t sigma in
  let rec aux l index =
    match l with
    | [] -> None
    | h :: t -> (
      let name = Names.Id.of_string ("th" ^ string_of_int index) in
      ignore (declare_eq name h);
      aux t (index + 1)
    ) in
  let _ = aux eqs 0 in
  str "Success."

let complete2 grefs =
  let terms = List.map (
    fun gref ->
      let cb = get_constant_body gref in
      My_term.from_constr cb.const_type) grefs in
  let res = Toma.complete_using_toma terms in
  let s = String.concat ", " (List.map My_term.to_trs res) in
  Pp.str s
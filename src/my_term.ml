open Names

type term =
| App of string * term list
| Var of string

(* (左辺, 右辺)で等式を表現する *)
type t = term * term

let variables t =
  let (t1, t2) = t in
  let rec variables_set = function
    | App (f, args) ->
      let args = List.map variables_set args in
      List.fold_left My_util.Set.union My_util.Set.empty args 
    | Var x -> My_util.Set.insert My_util.Set.empty x in
  let ids = My_util.Set.union (variables_set t1) (variables_set t2) in
  let consts = My_util.Set.from_list ["Knuth.Demo.e"] in
  My_util.Set.to_list (My_util.Set.exclude ids consts)


(* トップレベルのカンマで区切る *)
let split_by_comma s =
  let l = My_util.chars_of_string s in
  let rec loop acc l level =
    match l with
    | [] -> [acc]
      (* TODO: 現在0~1個のカンマ直後のスペースにしか対応していない *)
    | ',' :: ' ' :: t | ',' :: t ->
      if (level = 0) then acc :: (loop [] t level)
      else loop (acc @ [',']) t level
    | '(' :: t -> loop (acc @ ['(']) t (level + 1)
    | ')' :: t -> loop (acc @ [')']) t (level - 1)
    | h :: t -> loop (acc @ [h]) t level in
  List.map My_util.string_of_chars (loop [] l 0)

let rec term_of_string str =
  let reg = Str.regexp "^\\([^(]+\\)(\\(.+\\))$" in
  match Str.string_match reg str 0 with
  | true ->
    let f = Str.matched_group 1 str in
    let args_str = Str.matched_group 2 str in
    let args_str_list = split_by_comma args_str in
    let args = List.map term_of_string args_str_list in
    App (f, args)
  | false -> Var str

let from_trs s =
  match Str.split (Str.regexp " -> ") s with
  (* １つの等号がある等式しかサポートしない *)
  | [t1; t2] ->
    let t1 = term_of_string t1 in
    let t2 = term_of_string t2 in
    (t1, t2)
  | _ -> failwith ("Invalid input of TRS string: " ^ s)

let rec trs_string_of_term = function
  | App (f, args) ->
    let args = String.concat ", " (List.map trs_string_of_term args) in
    f ^ "(" ^ args ^ ")"
  | Var x -> x

let to_trs t = 
  let (t1, t2) = t in
  trs_string_of_term t1 ^ " -> " ^ trs_string_of_term t2

let rec constrexpr_of_term t =
  let open Constrexpr in
  match t with
| App (f, args) ->
  if f = "+" || f = "Knuth.Demo.f" then
    let terms = List.map constrexpr_of_term args in
    CAst.make (
      CNotation (
        None,
        (InConstrEntry, "_ + _"),
        (
          terms,
          [], [], []
        )
      )
    )
  else if f = "-" || f = "Knuth.Demo.i" then begin
    match List.map constrexpr_of_term args with
    | [term] ->
      CAst.make (
        CApp (
          CAst.make (CRef (Libnames.qualid_of_string "i", None)),
          [term, None]
        )
      )
      | _ -> failwith "Invalid term"
    end
  else failwith ("Not implemented" ^ f)
| Var x ->
  CAst.make (CRef (Libnames.qualid_of_string x, None))

let to_constrexpr (t1, t2) =
  let open Constrexpr in
  let vars = variables (t1, t2) in
  let t1 = constrexpr_of_term t1 in
  let t2 = constrexpr_of_term t2 in
  CAst.make (CApp (
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
                  ], [], [], []
                )
              )
            )
          )
        )
      ), None
    ]
  ))

open Constr

type term_or_eq =
| Term of term
| Eq of term * term

let from_constr c =
  let rec aux = function
  | Rel i -> Term (Var ("x" ^ string_of_int i))
  | Prod (_, _, t) ->
    aux (Constr.kind t)
  | App (f, args) ->
    (* f が "=" であるか *)
    let is_eq constr =
      begin
        match Constr.kind constr with
        | Ind (ind, _) ->
          let (mutind, i) = ind in
          let smut = Names.MutInd.to_string mutind in
            smut = "Coq.Init.Logic.eq"
        | _ -> false
      end in
    let args_t = List.map (
      fun t ->
        (match aux (Constr.kind t) with
         | Term t -> t
         | _ -> failwith "Invalid input: my_term.from_constr.case App args_t"
        )
      ) (Array.to_list args) in
    if is_eq f then
      let errs = String.concat " , " (List.map trs_string_of_term args_t) in
      begin match args_t with
      | _ :: arg1 :: arg2 :: _ ->
        Eq (arg1, arg2)
      | _ -> failwith ("Invalid input: my_term.from_constr.case App is_eq" ^ errs)
      end
    else
      begin match aux (Constr.kind f) with
      | Term (Var f_name) ->
        let args_list = args_t in
        Term (App (f_name, args_list))
      | _ -> failwith "Invalid input: match aux"
      end
  | Const (k, _) ->
    Term (Var (Names.Constant.to_string k))
  | _ -> failwith "Not implemented" in
  match aux (Constr.kind c) with
  | Eq (t1, t2) -> (t1, t2)
  | _ -> failwith "Invalid input: not Eq"
    


let pr t =
  let open Pp in
  str (to_trs t)
  
(* 各規則をTRS形式にした上で、その他の情報 (RULES, VARS 等) を追加した文字列を返す *)
(* TRS形式への変換では変数名を x0, x1, ... と固定するので、変数の数を数えて VARS を作る *)
let get_toma_input terms =
  let rec max l acc = match l with
    | [] -> acc
    | h :: t -> if h > acc then max t h else max t acc in
  (* 変数の最大個数 *)
  let num_vars = max (List.map (fun t -> List.length (My_term.variables t)) terms) 0 in
  let vars_string_of_num num = (* 例: 3 -> "x1 x2 x3" *)
    let rec loop acc n =
      if n = num then loop ("x" ^ string_of_int n) (n - 1)
      else if n > 0 then
        loop ("x" ^ string_of_int n ^ " " ^ acc) (n - 1) 
      else acc in
    loop "" num in
  let vars_string = vars_string_of_num num_vars in
  let rec string_of_terms = function
    | [] -> ""
    | hd :: tl -> My_term.to_trs hd ^ "\n" ^ (string_of_terms tl) in
  "(VAR " ^ vars_string ^ ")\n" ^
  "(RULES\n" ^ string_of_terms terms ^ ")\n"

(* 一時ファイルをmktempコマンドによって作成し、ファイル名を返す *)
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

(* ファイル名 f のファイルに s を書き込む *)
let write f s =
  let oc = open_out f in
  Printf.fprintf oc "%s" s;
  close_out oc

(* argfileをtomaに入力して、tomaの出力のうち関心のある部分を string list で返す *)
let toma argfile =
  (* tomaへのパスが通っている必要がある *)
  let command = "toma --ordered-completion " ^ argfile ^ " --v" in
  let ic = Unix.open_process_in command in
  let rec loop acc read =
    try
      let line = input_line ic in
      if read then
        if line = "" then loop acc false
        else loop (line :: acc) read
      else
        if line = "ES:" then loop acc true else loop acc false
    with
    | End_of_file -> acc in
  let res = loop [] false in
  ignore (Unix.close_process_in ic);
  (* 出力の先頭がリストの後ろに入っているので反転して返す *)
  List.rev res
  
let complete_using_toma terms =
  (* 一時ファイルに入力を書き込んで、tomaにファイル名を渡すことで完備化を行う *)
  let tmpfile = mktemp () in
  let _ = write tmpfile (get_toma_input terms) in
  let res = toma tmpfile in
  List.map My_term.from_trs res

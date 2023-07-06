let chars_of_string s =
  let rec aux i l =
    if i < 0 then l else aux (i - 1) (s.[i] :: l) in
  aux (String.length s - 1) []

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

module Set = struct
  type 'a t = 'a list

  let empty = []

  let to_list s = s

  let from_list l = l

  let rec insert l x =
    match l with
    | [] -> [x]
    | h :: t ->
      if h = x then l
      else h :: insert t x
    
  let rec delete l x =
    match l with
    | [] -> []
    | h :: t ->
      if h = x then t
      else h :: delete t x

  let rec union a b =
    match b with
    | [] -> a
    | h :: t -> union (insert a h) t

  let rec exclude a b =
    match b with
    | [] -> a
    | h :: t -> exclude (delete a h) t

end



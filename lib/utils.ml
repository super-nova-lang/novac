let explode s = s |> String.to_seq |> List.of_seq

let rec skip_whitespace input =
  match input with
  | head :: tail when head = ' ' || head = '\n' || head = '\t' -> skip_whitespace tail
  | rest -> rest
;;

let is_white c = c = ' ' || c = '\n' || c = '\t'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_numer c = c >= '0' && c <= '9'
let is_alnum c = is_alpha c || is_numer c
let is_ident c = is_alnum c || c = '\'' || c = '_'

let rec consume ~f acc = function
  | x :: xs when f x -> consume ~f (acc ^ String.make 1 x) xs
  | xs -> acc, xs
;;

let map_escape = function
  | 'n' -> '\n'
  | 'r' -> '\r'
  | 't' -> '\t'
  | 'b' -> '\b'
  | c -> c
;;

let rec consume_string acc = function
  | '"' :: xs -> acc, xs
  | '\\' :: c :: xs -> consume_string (acc ^ String.make 1 (map_escape c)) xs
  | c :: xs -> consume_string (acc ^ String.make 1 c) xs
  | [] -> failwith "Unclosed string"
;;

let consume_char = function
  | '\\' :: c :: '\'' :: xs -> map_escape c, xs
  | c :: '\'' :: xs -> c, xs
  | _ -> failwith "Invalid char literal"
;;

let rec consume_comment depth = function
  | '(' :: '*' :: xs -> consume_comment (depth + 1) xs
  | '*' :: ')' :: xs -> if depth = 1 then xs else consume_comment (depth - 1) xs
  | [] -> failwith "Unclosed comment"
  | _ :: xs -> consume_comment depth xs
;;

let read_entire_file filename =
  let ch = open_in_bin filename in
  let content = really_input_string ch (in_channel_length ch) in
  close_in ch;
  content
;;

let starts_with s prefix =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix
;;

let capitalize_first_letter s =
  if String.length s = 0
  then s
  else String.capitalize_ascii (String.sub s 0 1) ^ String.sub s 1 (String.length s - 1)
;;

let module_name_from_path file_path stdlib_dir =
  let open Filename in
  let stdlib_prefix = stdlib_dir ^ "/" in
  if starts_with file_path stdlib_prefix
  then (
    let relative_path =
      String.sub
        file_path
        (String.length stdlib_prefix)
        (String.length file_path - String.length stdlib_prefix)
    in
    let module_segments =
      relative_path
      |> remove_extension
      |> (fun s -> String.split_on_char '/' s)
      |> List.map capitalize_first_letter
    in
    "Std_" ^ String.concat "_" module_segments)
  else file_path |> basename |> remove_extension |> capitalize_first_letter
;;

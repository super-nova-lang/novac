open Token

exception Parser_unhandled of string
exception Parser_unexpected of Token.t * Token.loc

let rec parse ts =
  try parse' ts with
  | Parser_unhandled msg ->
    Logger.par#error "%s" msg;
    []

and parse' = function
  | [] -> []
  | [ (Eof, _) ] -> []
  | (t, l) :: _ -> raise (Parser_unexpected (t, l))
;;

type expr =
  | Number of float
  | Variable of string
  | Binary of char * expr * expr
  | Call of string * expr list
;;

type proto = Prototype of string * string array;;
type func = Function of proto * expr;;
type toplevel = TExtern of proto | TFunction of func;;

type expr = [
  | `Variable of string
  | `Number of float
  | `BinOp of char * expr * expr
  | `Call of string * expr list
  | `If of expr * expr * expr
  | `Var of string * expr option * expr
  | `For of string * expr * expr * expr option * expr
]

type proto = [ `Prototype of string * string list ]
type func = [ `Function of proto * expr ]

type toplevel = [ 
  | `TLMain of func 
  | `TLFunction of func
]

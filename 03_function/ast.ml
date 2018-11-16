type expr = [
  | `Number of float
  | `BinOp of char * expr * expr
]

type proto = [ `Prototype of string * string list ]
type func = [ `Function of proto * expr ]

type toplevel = [ 
  | `TLMain of func 
  | `TLFunction of func
]

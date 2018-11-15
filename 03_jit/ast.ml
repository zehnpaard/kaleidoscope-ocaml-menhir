type expr = [
  | `Number of float
  | `BinOp of char * expr * expr
]

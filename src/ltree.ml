type var = string

and expr =
  | E_var of var
  | E_lambda of var * typ * expr
  | E_apply of expr * expr
  | E_int of int

and typ = T_arrow of typ * typ | T_int [@@deriving show { with_path = false }]

type var = string

and expr =
  | E_var of var
  | E_lambda of var * typ * expr
  | E_apply of expr * expr

and typ = T_int | T_arrow of typ * typ [@@deriving show { with_path = false }]

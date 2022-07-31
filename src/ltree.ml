type var = string [@@deriving show { with_path = false }]

type expr = E_var of var | E_lambda of var * expr | E_apply of expr * expr
[@@deriving show { with_path = false }]

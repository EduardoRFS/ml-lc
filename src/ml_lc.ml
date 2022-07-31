let parse_expr expr = Lexer.from_string Parser.expr_opt expr |> Option.get
let apply = parse_expr "(lambda [f x] (f x))"
let () = Format.printf "%a\n%!" Ltree.pp_expr apply

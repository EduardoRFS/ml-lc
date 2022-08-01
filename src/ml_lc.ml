open Ltree

let parse_expr expr = Lexer.from_string Parser.expr_opt expr |> Option.get

let print_expr expr =
  let expr = parse_expr expr in
  Format.printf "%a\n%!" Ltree.pp_expr expr

let () = print_expr "x"
let () = print_expr "(lambda [(x: int)] x)"
let () = print_expr "(id id)"

type env = (var * value) list

and value = Closure of { env : env; param : var; body : expr } | Int of int
[@@deriving show { with_path = false }]

let rec interp env expr =
  match expr with
  | E_var var -> (
      match List.assoc_opt var env with
      | Some value -> value
      | None -> failwith "unknown variables")
  | E_lambda (param, _typ, body) -> Closure { env; param; body }
  | E_apply (lambda, arg) -> (
      let lambda = interp env lambda in
      let arg = interp env arg in

      match lambda with
      | Closure { env; param; body } ->
          let env = (param, arg) :: env in
          interp env body
      | Int _int -> failwith "TypeError")
  | E_int int -> Int int

let print_value expr =
  let expr = parse_expr expr in
  let value = interp [] expr in
  Format.printf "%a\n%!" pp_value value

let () = print_value "(lambda [(x: int)] x)"
let () = print_value "(lambda [(x: int)] 1)"

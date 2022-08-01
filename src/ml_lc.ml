open Ltree

let parse_expr expr = Lexer.from_string Parser.expr_opt expr |> Option.get

let print_expr expr =
  let expr = parse_expr expr in
  Format.printf "%a\n%!" Ltree.pp_expr expr

let () = print_expr "x"
let () = print_expr "(lambda [(x: int)] x)"
let () = print_expr "(id id)"

module Interp = struct
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
end

module Typer = struct
  open Ltree

  let rec pp_typ fmt typ =
    match typ with
    | T_arrow (param, return) -> (
        let needs_parens =
          match param with T_arrow _ -> true | T_int -> false
        in
        match needs_parens with
        | true -> Format.fprintf fmt "(%a) -> %a" pp_typ param pp_typ return
        | false -> Format.fprintf fmt "%a -> %a" pp_typ param pp_typ return)
    | T_int -> Format.fprintf fmt "int"

  let rec check t1 t2 =
    match (t1, t2) with
    | T_arrow (param1, return1), T_arrow (param2, return2) ->
        check param1 param2;
        check return1 return2
    | T_int, T_int -> ()
    | T_arrow _, T_int | T_int, T_arrow _ -> failwith "TypeClash"

  let rec typer env expr =
    match expr with
    | E_var var -> (
        match List.assoc_opt var env with
        | Some typ -> typ
        | None -> failwith "unknown variables")
    | E_lambda (param, param_typ, body) ->
        let env = (param, param_typ) :: env in
        let body_typ = typer env body in
        T_arrow (param_typ, body_typ)
    | E_apply (lambda, arg) -> (
        let lambda_typ = typer env lambda in
        let arg_typ = typer env arg in
        match lambda_typ with
        | T_arrow (param_typ, return_typ) ->
            check param_typ arg_typ;
            return_typ
        | T_int -> failwith "TypeError")
    | E_int _n -> T_int

  let print_typ expr =
    let expr = parse_expr expr in
    let typ = typer [] expr in
    Format.printf "%a\n%!" pp_typ typ

  let () = print_typ "(lambda [(x: int -> int)] (x 1))"
  let () = print_typ "(lambda [(a: int) (b: int)] b)"
end

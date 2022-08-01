%{
open Ltree
%}
%token <string> VAR (* x *)
%token <int> INT (* 1 *)
%token LAMBDA (* lambda *)
%token ARROW (* -> *)
%token COLON (* : *)
%token LPARENS (* ( *)
%token RPARENS (* ) *)
%token LBRACKET (* [ *)
%token RBRACKET (* ] *)

%token EOF

%start <Ltree.expr option> expr_opt
%start <Ltree.typ option> typ_opt

%%

let expr_opt :=
  | EOF;
    { None }
  | expr = expr; EOF;
    { Some expr }

let expr ==
  | expr_atom
  | expr_lambda
  | expr_apply

let expr_atom ==
  | expr_var
  | expr_int
  | expr_parens

let expr_var :=
  | var = VAR;
    { E_var var }
let expr_int :=
  | int = INT;
    { E_int int }
let expr_lambda :=
  | LAMBDA; LBRACKET; params = nonempty_list(param); RBRACKET; body = expr_atom;
    { List.fold_right (fun (param, typ) body -> E_lambda (param, typ, body) ) params body }
let param ==
  | LPARENS; param = VAR; COLON; typ = typ; RPARENS;
    { (param, typ) }
let expr_apply :=
  | lambda = expr_atom; arg = expr_atom;
    { E_apply (lambda, arg) }
  | lambda = expr_apply; arg = expr_atom;
    { E_apply (lambda, arg) }
let expr_parens :=
  | LPARENS; expr = expr; RPARENS;
    { expr }

let typ_opt :=
  | EOF;
    { None }
  | typ = typ; EOF;
    { Some typ }

let typ ==
  | typ_atom
  | typ_arrow

let typ_atom ==
  | typ_var
  | typ_parens

let typ_var :=
  | var = VAR;
    { if String.equal var "int" then
        T_int
      else failwith "TypeError" }
let typ_arrow :=
  | param = typ_atom; ARROW; return = typ_atom;
    { T_arrow (param, return) }
  | param = typ_atom; ARROW; return = typ_arrow;
    { T_arrow (param, return) }
let typ_parens :=
  | LPARENS; typ = typ; RPARENS;
    { typ }

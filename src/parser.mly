%{
open Ltree
%}
%token <string> VAR (* x *)
%token LAMBDA (* lambda *)
%token LPARENS (* ( *)
%token RPARENS (* ) *)
%token LBRACKET (* [ *)
%token RBRACKET (* ] *)

%token EOF

%start <Ltree.expr option> expr_opt

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
  | expr_parens

let expr_var :=
  | var = VAR;
    { E_var var }
let expr_lambda :=
  | LAMBDA; LBRACKET; params = nonempty_list(VAR); RBRACKET; body = expr_atom;
    { List.fold_right (fun param body -> E_lambda (param, body) ) params body }
let expr_apply :=
  | lambda = expr_atom; arg = expr_atom;
    { E_apply (lambda, arg) }
  | lambda = expr_apply; arg = expr_atom;
    { E_apply (lambda, arg) }
let expr_parens :=
  | LPARENS; expr = expr; RPARENS;
    { expr }

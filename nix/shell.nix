{ pkgs, ml-lc }:

with pkgs; with ocamlPackages; mkShell {
  inputsFrom = [ ml-lc ];
  packages = [
    # Make developer life easier
    # formatters
    nixfmt
    ocamlformat

    # OCaml developer tooling
    ocaml
    dune_3
    ocaml-lsp
    ocamlformat-rpc
  ];
}

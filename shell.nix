{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    nixfmt
    nix-linter
    rnix-lsp
  ];
}

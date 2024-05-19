{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    go
    golangci-lint
    gomodifytags
    gopls
    gotests
    gotools
    shellcheck
  ];
  shellHook = ''
    mkdir -p .go/tmp
  '';
}

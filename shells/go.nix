{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    go
    golangci-lint
    gomodifytags
    gopls
    gore
    gotests
    gotools
    nodejs
    shellcheck
  ];
  shellHook = ''
    mkdir -p .go/tmp
  '';
}

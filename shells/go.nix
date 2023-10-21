{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    go
    gocode
    golangci-lint
    gomodifytags
    gopls
    gore
    gotests
    gotools
    nodejs
    rnix-lsp
    shellcheck
  ];
  shellHook = ''
    mkdir -p .go/tmp
  '';
}

# ENVRC
# export GOPATH=$PWD/.go
# export GOTMPDIR=$PWD/.go/tmp

# use nix

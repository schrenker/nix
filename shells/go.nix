{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    go_1_19
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

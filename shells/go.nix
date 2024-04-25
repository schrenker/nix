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

# ENVRC
# export GOPATH=$PWD/.go
# export GOTMPDIR=$PWD/.go/tmp
#
# if ! has nix_direnv_version || ! nix_direnv_version 2.4.0; then
#     source_url "https://raw.githubusercontent.com/nix-community/nix-direnv/2.4.0/direnvrc" "sha256-XQzUAvL6pysIJnRJyR7uVpmUSZfc7LSgWQwq/4mBr1U="
# fi
# use flake

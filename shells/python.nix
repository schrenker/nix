{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    python39
    shellcheck
    virtualenv
  ];
  shellHook = ''
    virtualenv venv
    source venv/bin/activate
  '';
}

# ENVRC
#
# if ! has nix_direnv_version || ! nix_direnv_version 2.4.0; then
#     source_url "https://raw.githubusercontent.com/nix-community/nix-direnv/2.4.0/direnvrc" "sha256-XQzUAvL6pysIJnRJyR7uVpmUSZfc7LSgWQwq/4mBr1U="
# fi
# use flake

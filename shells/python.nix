{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    python39
    rnix-lsp
    shellcheck
    virtualenv
  ];
  shellHook = ''
    virtualenv venv
    source venv/bin/activate
  '';
}

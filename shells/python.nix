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

{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    go-jsonnet
    jsonnet-bundler
    jsonnet-language-server
  ];
}

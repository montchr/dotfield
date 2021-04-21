{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell rec {

  buildInputs = [
    rnix-lsp
    nixpkgs-fmt
  ];
}

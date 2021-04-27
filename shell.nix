{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let nixBin =
      writeShellScriptBin "nix" ''
        ${nixFlakes}/bin/nix --option experimental-features "nix-command flakes" "$@"
      '';
in mkShell {
  buildInputs = [
    git
    nix-zsh-completions
    nixpkgs-fmt
    rnix-lsp
  ];
  shellHook = ''
    export FLAKE="$(pwd)"
    export PATH="$FLAKE/bin:${nixBin}/bin:$PATH"
  '';
}

{ flake, pkgs, ... }:
let
  inherit (flake.perSystem.inputs') nix-ai-tools;
in

{
  home.packages = with nix-ai-tools.packages; [
    claude-code
    claude-desktop
    claudebox
    crush
  ];
}

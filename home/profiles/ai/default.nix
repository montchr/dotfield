{ flake, pkgs, ... }:
let
  inherit (flake.perSystem.inputs') nix-ai-tools;
in

{
  home.packages = with nix-ai-tools.packages; [
    # FIXME: seems they have not provided it as a package despite
    # mention in readme
    # backlog-md
    claude-code
    claude-desktop
    claudebox
    crush
  ];
}

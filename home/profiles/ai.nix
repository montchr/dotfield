{ flake, ... }:
{
  home.packages = with flake.perSystem.inputs'.nix-ai-tools.packages; [
    claude-code
    claude-desktop
    claudebox
    crush
  ];
}

{ moduleWithSystem, ... }:
{
  users.cdom.aspects.workstation.home = moduleWithSystem (
    perSystem@{ inputs' }:
    {
      home.packages = with perSystem.inputs'.nix-ai-tools.packages; [
        claude-code
        claude-desktop
        claudebox
        crush
      ];
    }
  );
}

{ moduleWithSystem, ... }:
{
  dotfield.users.cdom.aspects.ai.home = moduleWithSystem (
    perSystem@{ inputs', ... }:
    home@{ pkgs, ... }:
    {
      home.packages = with perSystem.inputs'.nix-ai-tools.packages; [
        # FIXME: seems they have not provided it as a package despite
        # mention in readme
        # backlog-md
        claude-code
        claude-desktop
        claudebox
        crush
      ];
    }
  );
}

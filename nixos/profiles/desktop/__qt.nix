{ flake, ... }:
{
  qt.enable = true;

  home-manager.sharedModules = [ "${flake.path}/home/profiles/desktop/qt.nix" ];
}

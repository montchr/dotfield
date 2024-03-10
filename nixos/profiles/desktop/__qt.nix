{ flake, ... }:
{
  qt.enable = true;

  home-manager.sharedModules = [ "${flake.self}/home/profiles/desktop/qt.nix" ];
}

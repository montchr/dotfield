{ flake, pkgs, ... }:
{
  programs.dconf.enable = true;

  environment.systemPackages = [ pkgs.dconf-editor ];

  home-manager.sharedModules = [ "${flake.self}/home/profiles/desktop/gtk/common.nix" ];
}

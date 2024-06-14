{ pkgs, ... }:
{
  imports = [ ../qt.nix ];

  home.packages = [ pkgs.kdePackages.okular ];
}

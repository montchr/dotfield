{ pkgs, ... }:
{
  home.packages = [
    pkgs.calibre
    pkgs.kdePackages.okular
    pkgs.mcomix
  ];
}

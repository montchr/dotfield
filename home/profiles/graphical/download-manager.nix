{ pkgs, ... }:
{
  home.packages = [
    pkgs.kdePackages.kget
  ];
}

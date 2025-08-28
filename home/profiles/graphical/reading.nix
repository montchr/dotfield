{ pkgs, ... }:
{
  imports = [
    ./applications/zathura.nix
  ];

  home.packages = [
    pkgs.calibre
    pkgs.kdePackages.okular
    pkgs.mcomix
  ];
}

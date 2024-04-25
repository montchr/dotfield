{ pkgs, ... }:
{
  imports = [
    ./nix-tools.nix
    ./nodejs.nix
  ];

  # TODO: add note about why is this useful?
  home.packages = [ pkgs.nodePackages.terser ];
}

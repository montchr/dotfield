{ pkgs, ... }:
{
  imports = [
    ./applications/calibre.nix
    ./applications/okular.nix
    ./applications/zathura.nix
  ];

  home.packages = [
    # TODO: for comic books -- pick one
    pkgs.mcomix
    pkgs.peruse
  ];
}

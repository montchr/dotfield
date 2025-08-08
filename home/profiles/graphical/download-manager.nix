{ pkgs, ... }:
{
  home.packages = [
    pkgs.kdePackages.kget

    # TODO: this is a (very pretty) bittorrent client, it should move somewhere else?
    pkgs.varia
  ];
}

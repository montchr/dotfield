{ pkgs, ... }:
{
  home.packages = [
    pkgs.rustup
    pkgs.rustic
    pkgs.rustlings
  ];
}

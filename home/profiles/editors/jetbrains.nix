# Requires further setup: <https://wiki.nixos.org/wiki/Jetbrains_Tools#JetBrains_Toolbox>
{ pkgs, ... }:
{
  home.packages = [
    pkgs.jetbrains.phpstorm

    # XXX: <2024-06-05 Wed 15:30> broken on aarch64-linux
    # pkgs.jetbrains-toolbox
  ];

  # FIXME: not mergeable!
  # programs.emacs.extraPackages =
}

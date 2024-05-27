{ lib, flake, ... }:
{
  home.packages = lib.singleton flake.packages.git-repo-manager;
}

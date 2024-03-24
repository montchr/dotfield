{ lib, flake, ... }:
{
  home.packages = lib.singleton flake.perSystem.packages.git-repo-manager;
}

{ lib, ... }:
{
  imports = [
    ./__beets-packages.nix
  ];

  perSystem =
    { pkgs, system, ... }:
    {
      packages = lib.filesystem.packagesFromDirectoryRecursive {
        inherit (pkgs) callPackage;
        directory = ./by-name;
      };
    };
}

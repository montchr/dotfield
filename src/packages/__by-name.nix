{ lib, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages = lib.filesystem.packagesFromDirectoryRecursive {
        inherit (pkgs) callPackage;
        directory = ./by-name;
      };
    };
}

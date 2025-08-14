{ lib, ... }:
let
  tree =
    root:
    let
      inherit (lib.strings) hasPrefix;
      inherit (lib.fileset) fileFilter intersection toList;
      nixFiles = fileFilter (f: f.hasExt "nix") root;
      publicFiles = fileFilter (f: !hasPrefix "_" f.name) root;
    in
    toList (intersection nixFiles publicFiles);
in
{
  imports =
    (tree ./hosts)
    ++ (tree ./lib)
    ++ (tree ./features)
    ++ (tree ./modules)
    ++ [
      ./packages
    ];

  flake.lib.fs = {
    importTree = tree;
  };
}

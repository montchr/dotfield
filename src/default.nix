{ lib, ... }:
let
  import-tree' =
    root:
    let
      nixFiles = lib.fileset.fileFilter (f: f.hasExt "nix") root;
      publicFiles = lib.fileset.fileFilter (f: !lib.hasPrefix "_" f.name) root;
    in
    lib.fileset.toList (lib.fileset.intersection nixFiles publicFiles);
in
{
  imports =
    (import-tree' ./lib)
    ++ (import-tree' ./features)
    ++ (import-tree' ./modules)
    ++ [
      ./packages
    ];
}

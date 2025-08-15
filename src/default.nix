{ lib, ... }:
let
  tree =
    root:
    let
      inherit (lib.strings) hasPrefix;
      inherit (lib.fileset)
        fileFilter
        intersection
        toList
        ;
      nixFiles = fileFilter (f: f.hasExt "nix") root;
      publicFiles = fileFilter (f: !hasPrefix "_" f.name) root;
    in
    (intersection nixFiles publicFiles);

  loadTree = root: lib.fileset.toList (tree root);

  loadUser =
    root:
    let
      inherit (lib.fileset) difference maybeMissing toList;
      base = tree root;
      features = maybeMissing (root + "/features");
      # FIXME: ideally we move this into metadata, this file is annoying.
      keysFile = (maybeMissing (root + "/keys.nix"));
    in
    toList (difference (difference base features) keysFile);
in
{
  imports =
    #    (loadTree ./hosts)
    (loadTree ./hosts/tuuvok)
    ++ (loadTree ./lib)
    ++ (loadTree ./features)
    ++ (loadTree ./modules)

    ++ (loadUser ./users/cdom)
    ++ (loadUser ./users/median)
    ++ (loadUser ./users/seadoom)
    ++ (loadUser ./users/zortflower)

    ++ [
      ./packages
    ];

  flake.lib.fs = {
    inherit loadTree loadUser tree;
  };
}

{ inputs, lib, ... }:
let
  tree' =
    root: globs:
    inputs.globset.lib.globs root (
      [
        "**/*.nix"
        "!**/_*" # private files
        "!**/_*/**" # private directories
      ]
      ++ globs
    );

  tree = root: tree' root [ ];
  loadTree = root: lib.fileset.toList (tree root);
  loadUser = root: lib.fileset.toList (tree' root [ "!/config/**" ]);
in
{
  imports =
    (loadTree ./hosts)
    ++ (loadTree ./lib)
    ++ (loadTree ./aspects)
    ++ (loadTree ./meta)
    ++ (loadTree ./modules)

    ++ (loadUser ./users/cdom)
    ++ (loadUser ./users/median)
    ++ (loadUser ./users/seadoom)
    ++ (loadUser ./users/zortflower)

    ++ [
      ./packages
    ];

  flake.lib.fs = {
    inherit
      loadTree
      loadUser
      tree
      tree'
      ;
  };
}

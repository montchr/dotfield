{
  inputs,
  lib,
  ...
}:
let
  inherit (inputs.globset.lib) globs;

  nixFilesFrom =
    root: extraGlobs:
    globs root (
      [
        "**/*.nix"
        "!**/_*" # private files
        "!**/_*/**" # private directories
      ]
      ++ extraGlobs
    );

  loadTree = root: loadTree' root [ ];
  loadTree' = root: globs: lib.fileset.toList (nixFilesFrom root globs);
  loadUsers =
    root:
    lib.fileset.toList (
      nixFilesFrom root [
        "!/*/config/**"
      ]
    );
in
{
  imports =
    (loadTree ./lib)
    ++ (loadTree ./features)
    ++ (loadTree ./hosts)
    ++ (loadTree ./modules)
    ++ (loadTree ./overlays)
    ++ (loadUsers ./users)
    ++ [
      ./meta
      ./packages
    ];

  flake.lib.fs = {
    inherit
      loadTree
      loadTree'
      loadUsers
      nixFilesFrom
      ;
  };
}

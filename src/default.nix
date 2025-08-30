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

  loadTree = root: lib.fileset.toList (nixFilesFrom root [ ]);
  loadUsers = root: lib.fileset.toList (nixFilesFrom root [ "!/*/config/**" ]);
in
{
  imports =
    (loadTree ./lib)
    ++ (loadTree ./hosts)
    ++ (loadTree ./modules)
    ++ (loadUsers ./users)
    ++ [
      ./meta
      ./packages
    ];

  flake.lib.fs = {
    inherit
      loadTree
      loadUsers
      nixFilesFrom
      ;
  };
}

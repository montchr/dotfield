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
  loadHosts = root: lib.fileset.toList (globs root [ "*/default.nix" ]);
  loadUsers = root: lib.fileset.toList (nixFilesFrom root [ "!/*/config/**" ]);
in
{
  imports =
    (loadTree ./lib)
    ++ (loadTree ./modules)
    ++ (loadHosts ./hosts)
    ++ (loadUsers ./users)
    ++ [
      ./meta
      ./packages
    ];

  flake.lib.fs = {
    inherit
      loadTree
      loadHosts
      loadUsers
      nixFilesFrom
      ;
  };
}

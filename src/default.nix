{
  inputs,
  lib,
  ...
}:
let
  nixFilesFrom =
    root: globs:
    inputs.globset.lib.globs root (
      [
        "**/*.nix"
        "!**/_*" # private files
        "!**/_*/**" # private directories
      ]
      ++ globs
    );

  loadTree = root: lib.fileset.toList (nixFilesFrom root [ ]);
  loadUser = root: lib.fileset.toList (nixFilesFrom root [ "!/config/**" ]);
in
{
  imports =
    (loadTree ./lib)
    ++ (loadTree ./modules)
    ++ [
      ./packages
    ];

  flake.lib.fs = {
    inherit loadTree loadUser nixFilesFrom;
  };
}

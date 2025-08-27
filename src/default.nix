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
  loadUser = root: lib.fileset.toList (nixFilesFrom root [ "!/config/**" ]);
in
{
  imports =
    (loadTree ./lib)
    ++ (loadTree ./modules)
    ++ (lib.fileset.toList (globs ./hosts [ "*/default.nix" ]))
    ++ [
      ./packages
    ];

  flake.lib.fs = {
    inherit loadTree loadUser nixFilesFrom;
  };
}

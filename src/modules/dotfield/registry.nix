# Copyright (C) 2025 Chris Montgomery
# SPDX-License-Identifier: GPL-2.0-or-later

{
  lib,
  config,
  ...
}:
let
  inherit (lib) mkOption types;
in
{
  options.dotfield = {
    registry = mkOption {
      type = types.submodule {
        options = {
          aspects = mkOption {
            type = types.attrsOf (
              types.submodule {
                options = {
                  transitiveImports = mkOption {
                    type = types.listOf types.str;
                    default = [ ];
                    description = "List of aspect names that this aspect transitively imports";
                  };
                };
              }
            );
            default = { };
            description = "Registry of all aspects and their transitive imports";
          };
        };
      };
      default = { };
      description = "Dotfield registry for aspect metadata and discovery";
    };
  };

  # Automatically populate the aspect registry by analyzing imports
  config.dotfield.registry.aspects = lib.mapAttrs (
    aspectName: aspect:
    let
      nixosImports = aspect.nixos.imports or [ ];
      homeImports = aspect.home.imports or [ ];
      transitiveImports =
        nixosImports ++ homeImports
        |> lib.concatMap (
          module:
          config.dotfield.aspects
          |> lib.attrNames
          |> lib.concatMap (
            name:
            let
              aspect = config.dotfield.aspects.${name};
            in
            lib.optionals (module == aspect.nixos || module == aspect.home) [ name ]
          )
        )
        |> lib.unique;
    in
    {
      inherit transitiveImports;
    }
  ) config.dotfield.aspects;
}

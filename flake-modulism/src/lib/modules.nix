# Copyright (C) 2024-2025 Chris Montgomery
# Copyright (C) 2025 Michael Belsanti
# SPDX-License-Identifier: GPL-2.0-or-later OR MIT

{ lib, ... }:
let
  inherit (lib) mkOption types;

  mkDeferredModuleOpt =
    description:
    mkOption {
      inherit description;
      type = types.deferredModule;
      default = { };
    };

  mkAspectNameOpt =
    name:
    mkOption {
      default = name;
      type = types.str;
      readOnly = true;
      internal = true;
      description = "Name of the aspect";
    };

  mkAspectListOpt =
    description:
    mkOption {
      type = types.listOf (
        types.submodule {
          options = {
            # This differs from `mkAspectNameOpt` in that it does not
            # set a default value inherited from the submodule's `name`
            # argument.  We avoid using `name` in this list context
            # because `name` will not reflect the original value as
            # inherited from the attrset where the aspect was originally
            # defined -- the latter `name` is what we want, not the
            # anonymous `name` from the list context.
            #
            # How does this not result in an error, you ask?  Because,
            # given project conventions, we *always* create an aspect
            # list from existing attributes where the desired name has
            # already been defined.  `_name` is sneakily set to the
            # original value because of this.  If, for some reason, you
            # were to manually define an aspect inside of an option
            # declared with this function (don't!), you would indeed run
            # into an error, and you would need to set `_name` manually.
            #
            # We can thank Claude(!) for this clever trick.
            _name = mkOption {
              type = types.str;
              readOnly = true;
              internal = true;
              description = "Name of the aspect";
            };
            nixos = mkDeferredModuleOpt "A NixOS module for this aspect";
            home = mkDeferredModuleOpt "A Home-Manager module for this aspect";
          };
        }
      );
      default = [ ];
    };
in
{
  flake.lib.modules = {
    inherit
      mkDeferredModuleOpt
      mkAspectListOpt
      mkAspectNameOpt
      ;
  };
}

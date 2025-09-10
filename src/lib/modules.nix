{
  lib,
  self,
  inputs,
  config,
  withSystem,
  ...
}:
let
  inherit (lib) mkOption types;
  inherit (lib)
    filter
    flatten
    map
    unique
    ;

  flakeSpecialArgs = {
    inherit self inputs config;
  };

  flakeSpecialArgs' =
    system:
    withSystem system (
      ctx@{ config, inputs', ... }:
      let
        perSystem = {
          inherit (ctx.config) legacyPackages packages;
          inherit inputs';
        };
      in
      flakeSpecialArgs // { inherit perSystem; }
    );

  collectTypedModules = type: lib.foldr (v: acc: acc ++ v.${type}.imports) [ ];
  collectNixosModules = collectTypedModules "nixos";
  collectHomeModules = collectTypedModules "home";
  collectNameMatches =
    own: others: own |> (map (v: others.${v.name} or null)) |> filter (v: v != null);
  # TODO: use ‘uniqueStrings’ once landed
  collectRequires = all: self: self |> map (v: v.requires) |> flatten |> unique |> map (v: all.${v});

  mkDeferredModuleOpt =
    description:
    mkOption {
      inherit description;
      type = types.deferredModule;
      default = { };
    };

  aspectSubmoduleGenericOptions = {
    # TODO: accept actual aspect shape
    requires = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "List of names of aspects required by this aspect";
    };
    nixos = mkDeferredModuleOpt "A NixOS module for this aspect";
    home = mkDeferredModuleOpt "A Home-Manager module for this aspect";
  };

  mkAspectNameOpt =
    name:
    mkOption {
      type = types.str;
      default = name;
      readOnly = true;
      internal = true;
    };

  mkAspectListOpt =
    description:
    mkOption {
      type = types.listOf (
        types.submodule {
          options = aspectSubmoduleGenericOptions // {
            # This differs from the `options.aspects.*.name` option
            # declaration in that it avoids setting a default value
            # inherited from the submodule's `name` argument.  We avoid
            # using `name` in this list context because `name` will not
            # reflect the original value as inherited from the attrset
            # where the aspect was originally defined -- the latter
            # `name` is what we want, not the anonymous `name` from the
            # list context.
            #
            # How does this not result in an error, you ask?  Because,
            # given project conventions, we *always* create an aspect
            # list from existing attributes where the desired name has
            # already been defined.  `name` is sneakily set to the
            # original value because of this.  If, for some reason, you
            # were to manually define an aspect inside of an option
            # declared with this function (don't!), you would indeed run
            # into an error, and you would need to set `name` manually.
            #
            # We can thank Claude(!) for this clever trick.
            name = mkOption {
              type = types.str;
              readOnly = true;
              internal = true;
              description = "Name of the aspect";
            };
          };
        }
      );
      default = [ ];
    };

in
{
  flake.lib.modules = {
    inherit
      aspectSubmoduleGenericOptions
      collectHomeModules
      collectNameMatches
      collectNixosModules
      collectRequires
      collectTypedModules
      flakeSpecialArgs
      flakeSpecialArgs'
      mkAspectListOpt
      mkAspectNameOpt
      mkDeferredModuleOpt
      ;
  };
}

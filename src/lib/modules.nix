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
    elem
    head
    filter
    tail
    map
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

  collectTypedModules = type: lib.foldr (v: acc: acc ++ (v.${type}.imports or [ ])) [ ];
  collectNixosModules = collectTypedModules "nixos";
  collectHomeModules = collectTypedModules "home";

  # Collects overlays from a list of aspects.
  # self.overlays.default should be appended last at the call site to allow overrides.
  collectOverlays = lib.foldr (v: acc: acc ++ (v.overlays or [ ])) [ ];
  collectNameMatches =
    own: others: own |> (map (v: others.${v.name} or null)) |> filter (v: v != null);
  collectRequires =
    aspects: roots:
    let
      rootNames = lib.catAttrs "name" roots;
      op =
        visited: toVisit:
        if toVisit == [ ] then
          visited
        else
          let
            cur = head toVisit;
            rest = tail toVisit;
          in
          if elem cur.name (map (v: v.name) visited) then
            op visited rest
          else
            let
              deps = map (name: aspects.${name}) (cur.requires or [ ]);
            in
            op (op visited deps ++ [ cur ]) rest;
    in
    (op [ ] roots) |> filter (v: !(lib.elem v.name rootNames));

  # Resolves the full list of user aspects given their spec and host context.
  # This consolidates the fragile aspect dependency resolution logic.
  resolveUserAspects =
    {
      username,
      userSpec,
      hostAspects,
    }:
    let
      userAspects = config.users.${username}.aspects;
      userAspectDeps =
        (collectRequires config.aspects userSpec.aspects)
        ++ (collectRequires userAspects userSpec.aspects);
      userExtendedAspects = collectNameMatches (
        hostAspects ++ userSpec.aspects ++ userAspectDeps
      ) userAspects;
      # NOTE: We only collect deps from config.aspects here, not from
      # userAspects. Extended aspects should define their own dependencies
      # directly. Attempting to source dependencies from multiple aspect
      # groups can cause attribute-not-found errors when one group lacks
      # the dependency. See the original comment in nixos.nix for details.
      userExtendedAspectsDeps = collectRequires config.aspects userExtendedAspects;
    in
    userSpec.aspects
    ++ userAspectDeps
    ++ userExtendedAspects
    ++ userExtendedAspectsDeps
    ++ [ (userAspects.core or { }) ];

  # Resolves all Home Manager modules for a user given their spec and host context.
  resolveUserHomeModules =
    {
      username,
      userSpec,
      hostAspects,
      baseHomeModules,
    }:
    let
      resolvedUserAspects = resolveUserAspects { inherit username userSpec hostAspects; };
    in
    baseHomeModules
    ++ (collectHomeModules hostAspects)
    ++ (collectHomeModules resolvedUserAspects)
    ++ (collectHomeModules config.users.${username}.baseline.aspects)
    ++ [ userSpec.configuration ];

  # Resolves all overlays for a user's standalone Home Manager configuration.
  # Collects from host aspects, user aspects, and appends self.overlays.default last.
  resolveUserOverlays =
    {
      username,
      userSpec,
      hostAspects,
    }:
    let
      resolvedUserAspects = resolveUserAspects { inherit username userSpec hostAspects; };
    in
    (collectOverlays hostAspects)
    ++ (collectOverlays resolvedUserAspects)
    ++ (collectOverlays config.users.${username}.baseline.aspects)
    ++ [ self.overlays.default ];

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
    overlays = mkOption {
      type = types.listOf (types.functionTo (types.functionTo types.attrs));
      default = [ ];
      description = "Nixpkgs overlays required by this aspect";
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
            # using `name` in this list context because it will not
            # reflect the original value of `name` as inherited from the
            # attrset where the aspect was originally defined -- the
            # latter `name` is what we want, not the anonymous `name`
            # from the list context.
            #
            # How does this not result in an error, you ask?  Because,
            # given project conventions, we *always* create an aspect
            # list from existing attributes where the desired name has
            # already been defined.  `name` is sneakily set to the
            # original value because of this.  If, for some reason, you
            # were to manually define an aspect inside of an option
            # declared with this function (don't!), you would indeed run
            # into an error, and you would need to set `name` manually.
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
      collectOverlays
      collectRequires
      collectTypedModules
      flakeSpecialArgs
      flakeSpecialArgs'
      mkAspectListOpt
      mkAspectNameOpt
      mkDeferredModuleOpt
      resolveUserAspects
      resolveUserHomeModules
      resolveUserOverlays
      ;
  };
}

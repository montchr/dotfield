{ lib, withSystem, ... }:
{
  flake.lib.colmena = {
    mkNode =
      evaled: _hostname: settings:
      let
        evaledModules = evaled._module.args.modules;
        settings' = {
          deployment = settings;
        };
        defaults = {
          deployment = {
            buildOnTarget = lib.mkDefault true;
          };
        };
      in
      {
        imports = evaledModules ++ [
          settings'
          defaults
        ];
      };

    metaFor = evaled: {
      meta = {
        # While it's not great to hard-code the system, for now, this option is
        # required, so we use the sane default nixpkgs for `x86_64-linux`.
        # In the end, this setting gets overridden on a per-host basis.
        nixpkgs = withSystem "x86_64-linux" (ctx: ctx.pkgs);
        description = "my personal machines";
        nodeNixpkgs = lib.mapAttrs (_: v: v.pkgs) evaled;
        nodeSpecialArgs = lib.mapAttrs (_: v: v._module.specialArgs) evaled;
      };
    };
  };
}

{
  ops,
  flake,
  withSystem,
  ...
}: let
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  mkNode = evaled: hostname: settings: let
    evaledModules = evaled._module.args.modules;
    settings' = {deployment = settings;};
    meta = ops.metadata.hosts.${hostname};
    defaults = {
      deployment = {
        buildOnTarget = l.mkDefault true;
        targetHost = l.mkDefault (
          if (meta ? "ipv6")
          then "${meta.ipv6.address}::1"
          else meta.ipv4.address
        );
        targetUser = l.mkDefault "cdom";
      };
    };
  in {imports = evaledModules ++ [settings' defaults];};

  metaFor = evaled: {
    meta = {
      # While it's not great to hard-code the system, for now, this option is
      # required, so we use the sane default nixpkgs for `x86_64-linux`.
      # In the end, this setting gets overridden on a per-host basis.
      nixpkgs = withSystem "x86_64-linux" (ctx: ctx.pkgs);
      description = "my personal machines";
      nodeNixpkgs = l.mapAttrs (_: v: v.pkgs) evaled;
      nodeSpecialArgs = l.mapAttrs (_: v: v._module.specialArgs) evaled;
    };
  };
}

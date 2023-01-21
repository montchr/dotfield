{self, ...}: let
  inherit (self) inputs systems;
  l = inputs.nixpkgs.lib // builtins;

  lib = l.makeExtensible (lself: let
    callLibs = file:
      import file {
        inherit l inputs;
        lib = lself;
        supportedSystems = systems;
      };
  in {
    nums = callLibs ./nums.nix;
    strings = callLibs ./strings.nix;
    systems = callLibs ./systems.nix;

    attrs = callLibs ./attrs.nix;
    modules = callLibs ./modules.nix;
    options = callLibs ./options.nix;

    typography = callLibs ./typography.nix;

    apps = {
      firefox = callLibs ./apps/firefox;
      fzf = callLibs ./apps/fzf;
      kitty = callLibs ./apps/kitty;
    };

    net = {
      uri = callLibs ./net/uri.nix;
      normalize-ip6-addr = callLibs ./net/normalize-ip6-addr.nix;
    };

    colors = callLibs ./colors;
    ids = callLibs ./ids;

    types =
      l.foldr (l: r: l // r) {
        # standalone functions
        fileLocation = callLibs ./types/_fileLocation.nix;
        host = callLibs ./types/_host.nix;
        net = callLibs ./types/_net.nix;
        user = callLibs ./types/_user.nix;
      } (l.attrValues {
        # loosely-grouped
        addrs = callLibs ./types/addrs.nix;
        keys = callLibs ./types/keys.nix;
        names = callLibs ./types/names.nix;
        numeric = callLibs ./types/numeric.nix;
      });
  });
in {flake = {inherit lib;};}

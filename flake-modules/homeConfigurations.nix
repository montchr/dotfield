{
  config,
  lib,
  flake-parts-lib,
  ...
}: let
  inherit
    (lib)
    mapAttrs
    mkOption
    optionalAttrs
    types
    ;
  inherit
    (flake-parts-lib)
    mkSubmoduleOptions
    mkPerSystemOption
    ;
in {
  options = {
    flake = mkSubmoduleOptions {
      homeConfigurations = mkOption {
        type = types.lazyAttrsOf (types.lazyAttrsOf types.raw);
        default = {};
        description = ''
          Per system an attribute set of standalone home-manager configurations.
          <literal>nix build .#&lt;name></literal> will build <literal>homeConfigurations.&lt;system>.&lt;name></literal>.
        '';
      };
    };

    perSystem = mkPerSystemOption (_: {
      _file = ./homeConfigurations.nix;
      options = {
        homeConfigurations = mkOption {
          type = types.lazyAttrsOf types.raw;
          default = {};
          description = ''
            An attribute set of standalone home-manager configurations to be built by <literal>nix build .#&lt;name></literal>.
            <literal>nix build .#&lt;name></literal> will build <literal>homeConfigurations.&lt;name></literal>.
          '';
        };
      };
    });
  };
  config = {
    flake.homeConfigurations =
      mapAttrs
      (_k: v: v.homeConfigurations or {})
      config.allSystems;

    perInput = system: flake:
      optionalAttrs (flake ? homeConfigurations.${system}) {
        homeConfigurations = flake.homeConfigurations.${system};
      };
  };
}

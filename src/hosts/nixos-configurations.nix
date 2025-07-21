# cf. https://github.com/mightyiam/infra/blob/d776bea2a6ef17a83327d13b0ab9900fff2f6408/modules/nixos-configurations.nix
{ lib, config, ... }:
let
  prefix = "nixosConfigurations/";
in
{
  flake.nixosConfigurations =
    config.flake.modules.nixos or { }
    |> lib.filterAttrs (name: _module: lib.hasPrefix prefix name)
    |> lib.mapAttrs' (
      name: module:
      let
        hostName = lib.removePrefix prefix name;
      in
      {
        name = hostName;
        value = lib.nixosSystem {
          modules = [
            module
            { networking = { inherit hostName; }; }
          ];
        };
      }
    );
}

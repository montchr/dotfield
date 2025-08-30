{
  config,
  lib,
  inputs,
  self,
  ...
}:
let
  nixos = self.outPath + "/nixos";

  overlays = (import (self.outPath + "/overlays/default.nix") { inherit inputs; });

  makeHost =
    hostName: hostConfig:
    let
      inherit (hostConfig) system;
      profiles = nixos + "/profiles";
      nixosModules =
        hostConfig.configuration.imports
        ++ (import (nixos + "/modules-list.nix"))
        ++ [
          inputs.home-manager.nixosModules.default
          inputs.sops-nix.nixosModules.sops
          inputs.stylix.nixosModules.stylix

          (profiles + "/core/default.nix")
          (profiles + "/networking/tailscale.nix")
        ];
    in
    inputs.${hostConfig.channel}.lib.nixosSystem {
      inherit system;
      modules = nixosModules ++ [
        {
          networking = { inherit hostName; };
          nixpkgs.config.allowUnfree = true;
          nixpkgs = { inherit overlays; };
        }
      ];
      specialArgs = {
        flake = self.lib.modules.flakeSpecialArgs' system;
      };
    };
in
{
  flake.nixosConfigurations = lib.mapAttrs makeHost config.hosts.nixos;
}

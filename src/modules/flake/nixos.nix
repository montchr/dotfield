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
    hostName: hostSpec:
    let
      inherit (hostSpec) system;

      profiles = nixos + "/profiles";

      nixosModules =
        hostSpec.configuration.imports
        ++ (import (nixos + "/modules-list.nix"))
        ++ [
          inputs.home-manager.nixosModules.default
          inputs.sops-nix.nixosModules.sops
          inputs.stylix.nixosModules.stylix

          (profiles + "/core/default.nix")
          (profiles + "/networking/tailscale.nix")
        ];

      homeModules = [
        hostSpec.baseline.home
      ];

      makeHome = username: userSpec: {
        imports = homeModules ++ [
          config.users.${username}.baseline.configuration
          userSpec.configuration
        ];
      };
    in
    inputs.${hostSpec.channel}.lib.nixosSystem {
      inherit system;
      modules = nixosModules ++ [
        {
          networking = { inherit hostName; };
          nixpkgs.config.allowUnfree = true;
          nixpkgs = { inherit overlays; };
          home-manager.users = lib.mapAttrs makeHome hostSpec.users;
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

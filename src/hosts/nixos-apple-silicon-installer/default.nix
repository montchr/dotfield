flake@{ self, inputs, ... }:
let
  mixins = self.outPath + "/nixos/mixins";
  profiles = self.outPath + "/nixos/profiles";

  configuration = self.nixosConfigurations.nixos-apple-silicon-installer;
in
{
  # FIXME
  # hosts.nixos.nixos-apple-silicon-installer = {
  #   system = "aarch64-linux";
  #   channel = "nixpkgs-apple-silicon";
  #   configuration = {
  #     imports = [
  #       "${inputs.nixos-apple-silicon}/iso-configuration"

  #       (mixins + "/installer.nix")
  #       (profiles + "/hardware/apple/apple-silicon.nix")

  #       # FIXME: must be perSystem
  #       #      { hardware.asahi.pkgsSystem = localSystem; }

  #     ];
  #   };
  # };

  # perSystem =
  #   { ... }:
  #   {
  #     packages.nixos-apple-silicon-installer = (
  #       configuration.config.system.build.isoImage.overrideAttrs (o: {
  #         # add ability to access the whole config from the command line
  #         passthru = (o.passthru or { }) // {
  #           inherit (configuration) config;
  #         };
  #       })
  #     );
  #   };
}

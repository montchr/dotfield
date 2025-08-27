flake@{ self, inputs, ... }:
let
  nixos = self.outPath + "/nixos";

  configuration = self.nixosConfigurations.nixos-apple-silicon-installer;
in
{
  hosts.nixos.nixos-apple-silicon-installer = {
    system = "aarch64-linux";
    channel = "nixpkgs-apple-silicon";
    configuration = {
      imports = [
        "${inputs.nixos-apple-silicon}/iso-configuration"

        (nixos + "/mixins/installer.nix")
        (nixos + "/hardware/apple/apple-silicon.nix")

        # FIXME: must be perSystem
        #      { hardware.asahi.pkgsSystem = localSystem; }

      ];
    };
  };

  perSystem =
    { ... }:
    {
      packages.nixos-apple-silicon-installer = (
        configuration.config.system.build.isoImage.overrideAttrs (o: {
          # add ability to access the whole config from the command line
          passthru = (o.passthru or { }) // {
            inherit (configuration) config;
          };
        })
      );
    };
}

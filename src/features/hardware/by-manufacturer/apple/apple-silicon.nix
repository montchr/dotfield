{ inputs, lib, ... }:
{
  aspects.hardware__apple__apple-silicon = {
    nixos = {
      imports = [
        inputs.nixos-apple-silicon.nixosModules.apple-silicon-support
      ];

      nixpkgs.overlays = lib.mkBefore [ inputs.nixos-apple-silicon.overlays.default ];

      hardware.asahi.enable = true;

      # U-Boot does not support EFI variables.
      boot.loader.efi.canTouchEfiVariables = lib.mkForce false;
      # U-Boot does not support switching console mode.
      boot.loader.systemd-boot.consoleMode = lib.mkForce "0";

      # Mutually exclusive legacy Apple hardware.
      hardware.facetimehd.enable = lib.mkForce false;
      services.mbpfan.enable = lib.mkForce false;
    };
  };
}

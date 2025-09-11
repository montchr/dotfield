{ inputs, lib, ... }:
{
  aspects.hardware__apple__apple-silicon = {
    nixos = {
      imports = [
        inputs.nixos-apple-silicon.nixosModules.apple-silicon-support
      ];

      hardware.asahi.enable = true;
      nixpkgs.overlays = lib.mkBefore [ inputs.nixos-apple-silicon.overlays.default ];

      boot.loader.systemd-boot.consoleMode = lib.mkForce "0";
      boot.loader.efi.canTouchEfiVariables = lib.mkForce false;

      # Mutually exclusive legacy Apple hardware.
      hardware.facetimehd.enable = lib.mkForce false;
      services.mbpfan.enable = lib.mkForce false;
    };
  };
}

{
  config,
  self,
  inputs,
  ...
}:
let
  nixos = self.outPath + "/nixos";
in
{
  hosts.nixos.ryosuke = {
    system = "x86_64-linux";
    aspects = with config.aspects; [ workstation ];
    configuration =
      { pkgs, ... }:
      {
        imports = [
          inputs.nixos-hardware.nixosModules.common-cpu-amd
          inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
          inputs.nixos-hardware.nixosModules.common-gpu-amd

          (nixos + "/mixins/gnome.nix")
          (nixos + "/mixins/jobwork.nix")

          (nixos + "/profiles/hardware/razer.nix")
          (nixos + "/profiles/remote-builders/default.nix")
        ];

        dotfield.guardian.enable = true;
        dotfield.guardian.username = "cdom";

        services.displayManager.autoLogin.enable = true;
        services.displayManager.autoLogin.user = "cdom";

        sops.defaultSopsFile = ./secrets/secrets.yaml;
        # Never remove old secrets (attempt to fix lockouts).
        sops.keepGenerations = 0;

        boot.loader.efi.canTouchEfiVariables = true;
        boot.initrd.supportedFilesystems = [ "btrfs" ];
        boot.supportedFilesystems = [ "btrfs" ];
        boot.kernelPackages = pkgs.linuxPackages_latest;

        time.timeZone = "America/New_York";
        networking.firewall.enable = true;

        services.tailscale.enable = true;

        programs.steam.enable = true;
        services.deluge.enable = true;
        environment.systemPackages = [ pkgs.jellyfin-media-player ];

        system.stateVersion = "22.05";
      };
  };
}

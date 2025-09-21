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
    aspects = with config.aspects; [
      workstation
      desktop-sessions__gnome
      development__kleinweb
    ];
    configuration =
      { pkgs, ... }:
      {
        imports = [
          inputs.nixos-hardware.nixosModules.common-cpu-amd
          inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
          inputs.nixos-hardware.nixosModules.common-gpu-amd
        ];

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

        system.stateVersion = "22.05";
      };
  };
}

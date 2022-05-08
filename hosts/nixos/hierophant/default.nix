{
  config,
  pkgs,
  lib,
  hmUsers,
  modulesPath,
  suites,
  profiles,
  ...
}:

let
  secretsDir = ../../../secrets;
in

{
  imports =
    (with suites; basic)
    ++ (with profiles; [users.seadoom])
    ++ [
      (modulesPath + "/profiles/qemu-guest.nix")
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.devices = ["/dev/sda"];

  networking.useDHCP = false;
  networking.interfaces.enp1s0.useDHCP = true;

  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "virtio_pci" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    # N.B. While nixos-generate-config will set this to a UUID by default, the
    # UUID of a disk on Hetzner Cloud appears to change if the server is
    # rebuilt. We know the root filesystem should always point to this
    # partition, so it's safer to point directly there.
    device = "/dev/sda1";
    fsType = "ext4";
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-id/scsi-0HC_Volume_19315958";
    fsType = "ext4";
    neededForBoot = true;
    options = [ "noatime" ];
  };

  swapDevices = [ ];

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # FIXME!
  networking.firewall.enable = false;

  # services.tailscale.enable = true;
  # networking.firewall.trustedInterfaces = [ "tailscale0" ];

  environment.variables.DOTFIELD_DIR = "/etc/dotfield";
  programs.mtr.enable = true; 
  programs.gnupg.agent.enable = true;

  services.openssh.enable = true;
  services.openssh.openFirewall = true;
  services.openssh.permitRootLogin = "prohibit-password";

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  users.mutableUsers = false;
  users.users.root.openssh.authorizedKeys.keys = (import "${secretsDir}/authorized-keys.nix");
  users.users.root.initialHashedPassword = "$6$HimRTytkPSPaFqxi$jqOi8wZhhDunVpHlAtBvOol6J.Gk3l0PeEdiLVkI.f3JoDwT4OixyOcetfz.X87.0m3PqJjU9OgllBTY919bx1";
  users.users.seadoom = {
    hashedPassword = "$6$OlgpB7UeQh/f7hi7$5Kq/fDAEXS01Qv1XynDaBr/SPjNicBPDBhXIsiWsdj76QdehPp3oJA5w8uueOz63UXajdCMw6tQFMvFn6d19Z1";
    openssh.authorizedKeys.keys = (import "${secretsDir}/authorized-keys.nix");
  };
  home-manager.users.seadoom = {
    config,
    suites,
    ...
  }: {
    imports = [hmUsers.seadoom];

    lib.dotfield.userPath = "/etc/dotfield";
  };

  system.stateVersion = "21.11";
}

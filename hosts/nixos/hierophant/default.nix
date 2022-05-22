{
  config,
  pkgs,
  lib,
  hmUsers,
  modulesPath,
  suites,
  profiles,
  primaryUser,
  ...
}: let
  secretsDir = ../../../secrets;
in {
  imports = with profiles; [
    environments.hetzner-cloud
    networking.tailscale
  ];

  networking.firewall.enable = false;
  networking.firewall.trustedInterfaces = ["enp1s0"];

  fileSystems."/nix" = {
    device = "/dev/disk/by-id/scsi-0HC_Volume_19315958";
    fsType = "ext4";
    neededForBoot = true;
    options = ["noatime"];
  };

  environment.variables.DOTFIELD_DIR = "/etc/dotfield";
  programs.mtr.enable = true;
  programs.gnupg.agent.enable = true;

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  users.mutableUsers = false;
  users.users.root.openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
  users.users.root.initialHashedPassword = "$6$HimRTytkPSPaFqxi$jqOi8wZhhDunVpHlAtBvOol6J.Gk3l0PeEdiLVkI.f3JoDwT4OixyOcetfz.X87.0m3PqJjU9OgllBTY919bx1";
  users.users.seadoom = {
    extraGroups = ["wheel"];
    hashedPassword = "$6$OlgpB7UeQh/f7hi7$5Kq/fDAEXS01Qv1XynDaBr/SPjNicBPDBhXIsiWsdj76QdehPp3oJA5w8uueOz63UXajdCMw6tQFMvFn6d19Z1";
    isNormalUser = true;
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
  };
  home-manager.users.seadoom = hmArgs: {
    imports = [hmUsers.seadoom];

    lib.dotfield.userPath = "/etc/dotfield";
  };

  system.stateVersion = "21.11";
}

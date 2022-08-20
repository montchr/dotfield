{
  config,
  lib,
  pkgs,
  profiles,
  suites,
  ...
}: {
  imports =
    (with suites;
      tangible
      ++ workstation)
    ++ (with (profiles.shared); [
      login.gdm
    ]);

  boot.loader.systemd-boot.enable = true;

  # Will be overridden by the bootstrapIso module.
  fileSystems."/" = {device = "/dev/disk/by-label/nixos";};

  users.users.nixos = {
    password = "nixos";
    description = "default";
    isNormalUser = true;
    extraGroups = ["wheel"];
  };

  home-manager.users.nixos = hmArgs: {
    imports = with hmArgs.roles; graphical;
  };

  system.stateVersion = "22.05";
}

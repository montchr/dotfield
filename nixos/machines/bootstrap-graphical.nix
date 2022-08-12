{
  config,
  hmUsers,
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
    ++ (with profiles; [
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
    imports = with hmArgs.suites; basic ++ graphical;
  };

  system.stateVersion = "22.05";
}

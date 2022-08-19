{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [./common.nix];

  boot.loader.grub.enable = false;
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.consoleMode = "auto";
}

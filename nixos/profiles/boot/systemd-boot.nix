{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [./common.nix];

  boot.loader = {
    grub.enable = false;
    systemd-boot = {
      enable = true;
      consoleMode = "auto";
      configurationLimit = lib.mkDefault 24;
      editor = false;
    };
  };
}

{
  config,
  lib,
  pkgs,
  ...
}: {
  boot = {
    loader = {
      timeout = lib.mkForce 4;
      # TODO: enable grub?
      grub.enable = false;
      systemd-boot.enable = pkgs.stdenv.hostPlatform.system == "x86_64-linux";
    };
    kernelParams =
      ["modeset" "nofb"]
      ++ lib.optionals (pkgs.stdenv.hostPlatform.system == "x86_64-linux") [
        "pti=off"
        "spectre_v2=off"
      ];

    kernelPackages = pkgs.linuxPackages_latest;

    consoleLogLevel = 3;
    kernel.sysctl."vm.swappiness" = 0;
    kernel.sysctl."kernel/sysrq" = 1;
  };
}
## sources:
# https://github.com/balsoft/nixos-config/tree/b5ed51152f96225c0bb14482bdb3022b9c979679/profiles/boot.nix


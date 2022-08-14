{
  config,
  lib,
  pkgs,
  ...
}: {
  boot = {
    kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;
    consoleLogLevel = lib.mkDefault 3;

    # TODO: provide reasoning.
    # kernel.sysctl."vm.swappiness" = 0;
    # kernel.sysctl."kernel/sysrq" = 1;
  };
}
## sources:
# https://github.com/balsoft/nixos-config/tree/b5ed51152f96225c0bb14482bdb3022b9c979679/profiles/boot.nix

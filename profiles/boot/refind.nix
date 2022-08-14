# FIXME: refind gets wiped and becomes unavailable for some reason...
{
  config,
  lib,
  pkgs,
  ...
}: {
  assertions = [
    {
      assertion = config.boot.loader.grub.efiSupport -> config.boot.systemd-boot.enable;
      message = "rEFInd is only compatible with EFI boot!";
    }
  ];

  environment.systemPackages = with pkgs; [
    refind
  ];
}

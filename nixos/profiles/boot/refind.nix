# FIXME: refind gets wiped and becomes unavailable for some reason...
#        probable fix: set `boot.loader.efi.canTouchEfiVariables` to false
#        srvos keeps it disabled by default, noting that it should only be enabled during installation.
{
  config,
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

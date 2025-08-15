{
  dotfield.hosts.nixos.boschic.nixos = {
    # FIXME: disable. likely interferes with rEFInd.
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.timeout = 15;
    boot.initrd.supportedFilesystems = [ "btrfs" ];
    boot.supportedFilesystems = [ "btrfs" ];
  };
}

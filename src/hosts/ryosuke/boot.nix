{
  dotfield.hosts.nixos.ryosuke.nixos =
    { pkgs, ... }:
    {
      boot.loader.efi.canTouchEfiVariables = true;
      boot.initrd.supportedFilesystems = [ "btrfs" ];
      boot.supportedFilesystems = [ "btrfs" ];
      boot.kernelPackages = pkgs.linuxPackages_latest;
    };
}

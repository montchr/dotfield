{ config, lib, pkgs, ... }:

{
  # LISH compatibility
  boot.kernelParams = [ "console=ttyS0,19200n8" ];
  boot.loader.timeout = 10;
  boot.loader.grub.forceInstall = true;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.extraConfig = ''
    serial --speed=19200 --unit=0 --word=8 --parity=no --stop=1;
    terminal_input serial;
    terminal_output serial
  '';

  fileSystems."/" = {
    device = "/dev/sda";
    fsType = "ext4";
  };

  swapDevices = [
    { device = "/dev/sdb"; }
  ];

  # Linode networking guides assume an interface of `eth0` because linodes will
  # have a single networking interface.
  networking.usePredictableInterfaceNames = false;
  # Disable DHCP globally as we will not need it.
  networking.useDHCP = false;
  # Enable DHCP for the single networking interface.
  networking.interfaces.eth0.useDHCP = true;

  # Diagnostic tools used by Linode support.
  environment.systemPackages = with pkgs; [
    inetutils
    metr
    sysstat
  ];
}

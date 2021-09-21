{ suites, ... }:
{
  ### root password is empty by default ###
  imports = suites.linode;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;

}

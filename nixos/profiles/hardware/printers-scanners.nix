{
  config,
  lib,
  pkgs,
  ...
}: {
  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = with pkgs; [
    epson-201212w # EPSON WF-3520
  ];
  nixpkgs.config.allowUnfree = lib.mkForce true;
  networking.extraHosts = ''
    192.168.1.192 EPSON08CB87.local
  '';
}

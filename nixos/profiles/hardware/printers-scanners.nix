{
  config,
  lib,
  pkgs,
  ...
}: {
  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = with pkgs; [
    # EPSON WF-3520
    epson-201212w
  ];
  nixpkgs.config.allowUnfree = lib.mkForce true;
  networking.extraHosts = ''
    192.168.1.192 EPSON08CB87.local
  '';
}

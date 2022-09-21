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
    gutenprint
  ];
  nixpkgs.config.allowUnfree = lib.mkForce true;
}

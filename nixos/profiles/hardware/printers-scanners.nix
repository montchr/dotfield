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
    gutenprint
  ];
  nixpkgs.config.allowUnfree = lib.mkForce true;
}

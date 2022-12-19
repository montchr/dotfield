{
  inputs,
  packages,
  ...
}: let
  l = inputs.nixpkgs.lib // builtins;
in {
  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [
    packages.epson-201212w # <- EPSON WF-3520
  ];
  nixpkgs.config.allowUnfree = l.mkForce true;
  networking.extraHosts = ''
    192.168.1.192 EPSON08CB87.local
  '';
}

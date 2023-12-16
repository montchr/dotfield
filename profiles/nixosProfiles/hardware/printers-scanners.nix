{
  lib,
  flake,
  ...
}: let
  inherit (flake.perSystem) packages;
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers =
    lib.optional (packages ? "epson-201212w") packages.epson-201212w
    # <- EPSON WF-3520
    ;
  networking.extraHosts = ''
    192.168.1.192 EPSON08CB87.local
  '';
}

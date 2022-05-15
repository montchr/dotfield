{
  config,
  lib,
  pkgs,
  ...
}: {
  services.printing.enable = true;
  services.printing.drivers = with pkgs; [];

  hardware.sane.enable = true;
  hardware.sane.extraBackends = with pkgs; [sane-airscan];
}

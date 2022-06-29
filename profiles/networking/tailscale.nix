{
  config,
  lib,
  pkgs,
  ...
}: {
  services.tailscale.enable = true;
  networking.firewall.trustedInterfaces = ["tailscale0"];
  networking.firewall.checkReversePath = "loose";
}

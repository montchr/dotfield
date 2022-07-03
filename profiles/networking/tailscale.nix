{
  config,
  lib,
  pkgs,
  ...
}: {
  services.tailscale.enable = true;
  networking.firewall.trustedInterfaces = [config.services.tailscale.interfaceName];
  networking.firewall.checkReversePath = "loose";
}

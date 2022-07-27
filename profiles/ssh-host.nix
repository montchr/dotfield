{
  config,
  lib,
  pkgs,
  ...
}: {
  services.openssh.enable = lib.mkForce true;
}

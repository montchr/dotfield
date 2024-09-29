{ lib, ... }:
{
  imports = [ ../profiles/remote-builders ];

  dotfield.guardian.enable = false;

  # HACK: Override core profile
  services.openssh.settings.PermitRootLogin = lib.mkForce "yes";
}

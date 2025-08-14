{ self, lib, ... }:
{
  dotfield.features.installer.nixos = {
    imports = [
      self.dotfield.features.nixos.remote-builders
    ];

    dotfield.guardian.enable = false;

    # HACK: Override core profile
    services.openssh.settings.PermitRootLogin = lib.mkForce "yes";
  };
}

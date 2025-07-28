{ self, lib, ... }:
{
  dotfield.modules.installer.nixos = {
    imports = [
      self.dotfield.modules.nixos.remote-builders
    ];

    dotfield.guardian.enable = false;

    # HACK: Override core profile
    services.openssh.settings.PermitRootLogin = lib.mkForce "yes";
  };
}

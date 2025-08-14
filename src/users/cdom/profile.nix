{ self, ... }:
{
  dotfield.users.cdom = { };

  dotfield.hosts.nixos.tuuvok.users.cdom = {
    wayland.windowManager.sway.config.startup = [
      {
        command = "teams-for-linux";
      }
    ];
  };

  # FIXME: this will affect the global module unless something is done
  # to scope it to the user.
  dotfield.features.workstation.home = {
    imports = with self.dotfield.features.home; [
      "gpg/with-ssh"
      password-store
    ];
  };
}

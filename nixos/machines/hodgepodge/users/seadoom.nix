# FIXME: make reusable -- copied wholesale from cdom@ryosuke
{
  self,
  config,
  primaryUser,
  ...
}: let
  inherit (self.inputs.digga.lib) rakeLeaves;

  username = "seadoom";

  ownProfiles = rakeLeaves (self + "/home/users/cdom/profiles");
in {
  sops.secrets."users/${username}/passphrase".neededForUsers = true;

  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    passwordFile = config.sops.secrets."users/${username}/passphrase".path;
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
  };

  # Enable automatic login for the user.
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = username;

  # Workaround for GNOME autologin: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;

  home-manager.users.${username} = hmArgs: {
    imports =
      hmArgs.roles.workstation
      ++ [ownProfiles.work];
    home.stateVersion = "21.11";
  };
}

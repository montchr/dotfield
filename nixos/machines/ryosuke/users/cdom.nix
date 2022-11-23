# FIXME: make reusable -- duplicated as seadoom@hodgepodge
{
  self,
  config,
  lib,
  primaryUser,
  ...
}: let
  inherit (self.inputs.digga.lib) rakeLeaves;

  username = "cdom";

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
      (with hmArgs.roles; workstation)
      ++ (with ownProfiles; [work]);
    home.stateVersion = "22.05";
  };
}

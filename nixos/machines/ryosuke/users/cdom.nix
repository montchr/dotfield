{
  self,
  config,
  lib,
  pkgs,
  primaryUser,
  ...
}: let
  inherit (self.inputs.digga.lib) rakeLeaves;
  inherit (config.lib.dotfield) srcPath;

  username = "cdom";

  ownProfiles = rakeLeaves (srcPath + "/home/users/${username}/profiles");
in {
  sops.secrets."users/${username}/passphrase".neededForUsers = true;

  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    passwordFile = config.sops.secrets."users/${username}/passphrase".path;
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
    extraGroups =
      [
        "wheel"
        "video"
        "audio"
        "seadome"
        "secrets"
        "keys"
      ]
      ++ (lib.optional config.networking.networkmanager.enable "networkmanager")
      ++ (lib.optional config.services.mysql.enable "mysql")
      ++ (lib.optional config.virtualisation.docker.enable "docker")
      ++ (lib.optional config.virtualisation.podman.enable "podman")
      ++ (lib.optional config.virtualisation.libvirtd.enable "libvirtd")
      ++ (lib.optional config.virtualisation.virtualbox.host.enable "vboxusers");
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
    # FIXME: this must be set everywhere!
    programs.home-manager.enable = true;
  };
}

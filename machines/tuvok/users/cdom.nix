{
  config,
  ops,
  ...
}: let
  username = "cdom";
in {
  sops.secrets."user-${username}-password".neededForUsers = true;

  users.users.${username} = {
    isNormalUser = true;
    passwordFile = config.sops.secrets."user-${username}-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };

  # Enable automatic login for the user.
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = username;

  # Workaround for GNOME autologin: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;

  home-manager.users.${username} = hmArgs: {
    imports = with hmArgs.roles; workstation;
    home.stateVersion = "23.05";
  };
}

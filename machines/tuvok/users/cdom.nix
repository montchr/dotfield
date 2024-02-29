{
  config,
  ops,
  pkgs,
  ...
}: let
  username = "cdom";
in {
  sops.secrets."user-${username}-hashed-password".neededForUsers = true;

  users.users.${username} = {
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."user-${username}-hashed-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
    shell = pkgs.zsh;
  };

  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = username;

  home-manager.users.${username} = {roles, ...}: {
    imports =
      roles.workstation
      ++ [
        {
          # The trackpad on this device is huge, and I always end up touching
          # its corner with my palm, which is very disruptive.
          dconf.settings."org/gnome/desktop/peripherals/touchpad".tap-to-click = true;
        }
      ];
    home.stateVersion = "23.05";
  };
}

{
  config,
  ops,
  pkgs,
  ...
}:
let
  username = "cdom";
in
{
  sops.secrets."users/${username}/hashed-password".neededForUsers = true;

  users.users.${username} = {
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/${username}/hashed-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
    shell = pkgs.bashInteractive;
  };

  home-manager.users.${username} =
    { profiles, features, ... }:
    {
      imports = features.workstation ++ [
        profiles.desktop.applications.microsoft-teams
        profiles.development.work.default
        profiles.gpg.with-ssh-support
        profiles.shells.fish.trampoline
        profiles.shells.prompts.starship.default

        {
          # The trackpad on this device is huge, and I always end up touching
          # its corner with my palm, which is very disruptive.
          dconf.settings."org/gnome/desktop/peripherals/touchpad".tap-to-click = false;
        }
      ];
      home.stateVersion = "23.05";
    };
}

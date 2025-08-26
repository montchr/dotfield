flake:
let
  inherit (flake.config.dotfield) meta;

  hostAspects = flake.config.dotfield.hosts.nixos.tuuvok.aspects;
in
{
  dotfield.hosts.nixos.tuuvok = {
    users.cdom = {
      aspects = hostAspects;

      home = {
        # FIXME: this no longer works!  causes error
        # programs.firefox.profiles.work.isDefault = true;
        # programs.firefox.profiles.home.isDefault = false;

        wayland.windowManager.sway.config.startup = [
          { command = "teams-for-linux"; }
        ];

        home.stateVersion = "23.05";
      };
    };

    nixos =
      {
        config,
        pkgs,
        ...
      }:
      let
        username = "cdom";
      in
      {
        sops.secrets."users/${username}/hashed-password".neededForUsers = true;

        users.users.${username} = {
          uid = 1000;
          isNormalUser = true;
          hashedPasswordFile = config.sops.secrets."users/${username}/hashed-password".path;
          openssh.authorizedKeys.keys = meta.users.cdom.keys.ssh;
          shell = pkgs.bashInteractive;
        };
      };
  };
}

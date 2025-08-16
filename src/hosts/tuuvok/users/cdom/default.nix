flake:
let
  inherit (flake.config.dotfield) meta;
in
{
  dotfield.hosts.nixos.tuuvok = {
    users.cdom = {
      features =
        flake.config.dotfield.hosts.nixos.tuuvok.features
        ++ (with flake.config.dotfield.users.cdom.features; [
          ai
          mail
          music-production
        ])
        ++ (with flake.config.dotfield.features; [
          git__with-gpg-signing
          gpg__with-ssh
          jujutsu__with-gpg-signing
          jujutsu__with-sign-on-push
        ]);

      home = {
        # imports = (
        #   with flake.config.dotfield.users.cdom.features;
        #   [
        #     ai.home
        #     mail.home
        #     music-production.home

        #     git__with-gpg-signing.home
        #     gpg__with-ssh-support.home
        #     jujutsu__with-gpg-signing.home
        #     jujutsu__with-sign-on-push.home
        #   ]
        # );
        programs.firefox.profiles.work.isDefault = true;
        programs.firefox.profiles.home.isDefault = false;

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

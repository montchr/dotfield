flake@{ ... }:
{
  hosts.nixos.riebeck = {
    configuration =
      {
        config,
        pkgs,
        ...
      }:
      let
        username = "cdom";
      in
      {
        #sops.secrets."users/${username}/hashed-password".neededForUsers = true;

        users.users.${username} = {
          uid = 1000;
          isNormalUser = true;
          # hashedPasswordFile = config.sops.secrets."users/${username}/hashed-password".path;
          openssh.authorizedKeys.keys = flake.config.meta.users.cdom.keys.ssh;
          extraGroups = [ "wheel" ];
        };
      };

    users.cdom = {
      aspects = with flake.config.users.cdom.aspects; [
        # mail
        # music-production
      ];
      configuration =
        { pkgs, ... }:
        {
          # FIXME: maybe?
          # programs.firefox.profiles.work.isDefault = true;
          # programs.firefox.profiles.home.isDefault = false;

          programs.jujutsu.signing.gpg.enable = true;
          programs.jujutsu.signing.onPush = true;

          home.packages = with pkgs; [
            zed-editor
          ];

          home.stateVersion = "25.05";
        };
    };
  };
}

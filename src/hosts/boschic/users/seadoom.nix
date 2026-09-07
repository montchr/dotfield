flake@{ self, ... }:
{
  hosts.nixos.boschic = {
    configuration =
      {
        pkgs,
        config,
        ...
      }:
      {
        sops.secrets."users/seadoom/hashed-password".neededForUsers = true;

        users.users.seadoom = {
          uid = 1000;
          isNormalUser = true;
          hashedPasswordFile = config.sops.secrets."users/seadoom/hashed-password".path;
          openssh.authorizedKeys.keys = flake.config.meta.users.cdom.keys.ssh;
          extraGroups = [ "wheel" ];
        };
      };

    users.seadoom = {
      configuration = {
        programs.git.signing.signByDefault = true;
        programs.jujutsu.signing.gpg.enable = true;
        programs.jujutsu.signing.onPush = true;

        programs.rclone.remotes."whatbox".mounts."".enable = true;

        home.stateVersion = "21.11";
      };
    };
  };
}

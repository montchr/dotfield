flake@{ self, ... }:
{
  hosts.nixos.hodgepodge = {
    configuration =
      {
        config,
        pkgs,
        ...
      }:
      let
        username = "seadoom";
      in
      {
        sops.secrets."users/${username}/hashed-password".neededForUsers = true;

        users.users.${username} = {
          uid = 1000;
          isNormalUser = true;
          hashedPasswordFile = config.sops.secrets."users/${username}/hashed-password".path;
          openssh.authorizedKeys.keys = flake.config.meta.users.cdom.keys.ssh;
          extraGroups = [
            "audio"
            "video"
          ];
        };

        services.displayManager.autoLogin.enable = true;
        services.displayManager.autoLogin.user = username;
      };

    users.cdom = {
      configuration = {
        imports = [
          (self.outPath + "/home/mixins/workstation.nix")
        ];

        programs.git.signing.signByDefault = true;

        home.stateVersion = "21.11";
      };
    };
  };

}

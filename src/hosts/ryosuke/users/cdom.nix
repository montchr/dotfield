{
  hosts.nixos.ryosuke = {
    configuration =
      { flake, config, ... }:
      let
        username = "cdom";
      in
      {
        users.users.${username} = {
          uid = 1000;
          isNormalUser = true;
          openssh.authorizedKeys.keys = flake.config.meta.users.cdom.keys.ssh;
          extraGroups = [ "wheel" ];
        };
      };

    users.cdom = {
      configuration = {
        programs.git.signing.signByDefault = true;
        home.stateVersion = "22.05";
      };
    };
  };
}

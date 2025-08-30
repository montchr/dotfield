{ self, ... }:
let
  mixins = self.outPath + "/home/mixins";
  profiles = self.outPath + "/home/profiles";
in
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
        };
      };

    users.cdom = {
      configuration = {
        imports = [
          (mixins + "/jobwork.nix")
          (mixins + "/workstation.nix")
        ];

        programs.git.signing.signByDefault = true;

        home.stateVersion = "22.05";
      };
    };
  };
}

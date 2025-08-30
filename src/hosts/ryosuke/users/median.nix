{ self, ... }:
{
  hosts.nixos.ryosuke.configuration =
    let
      username = "median";
    in
    {
      users.users.${username} = {
        uid = 1001;
        isNormalUser = true;
        # TODO: generate
        # openssh.authorizedKeys.keys = flake.config.meta.users.${username}.keys.ssh;
      };
    };

  hosts.nixos.ryosuke.users.median = {
    configuration = {
      imports = [
        (self.outPath + "/home/mixins/graphical.nix")
      ];

      home.stateVersion = "24.05";
    };
  };
}

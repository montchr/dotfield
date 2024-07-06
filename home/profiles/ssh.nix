{ ops, config, ... }:
let
  inherit (config.home) homeDirectory;
  inherit (ops) hosts;
in
{
  programs.ssh = {
    enable = true;
    forwardAgent = false;
    serverAliveInterval = 300;
    controlMaster = "auto";
    controlPersist = "10m";
    matchBlocks = {
      "gabbro".hostname = "${hosts.gabbro.ipv6.address}::1";
      "hierophant".hostname = "${hosts.hierophant.ipv6.address}::1";
      "moraine".hostname = "${hosts.moraine.ipv6.address}::1";
      "synoxyn" = {
        hostname = hosts.synoxyn.ipv4.address;
        port = 2367;
      };

      "github.com" = {
        # inherit identityFile;
        # identitiesOnly = true;
        user = "git";
      };

      "eu.nixbuild.net" = {
        identityFile = "${homeDirectory}/.ssh/id_ed25519_seadome_nixbuild_net";
        identitiesOnly = true;
        extraOptions = {
          PubkeyAcceptedKeyTypes = "ssh-ed25519";
        };
      };
    };

    includes = [ "~/.config/ssh/config.local" ];
  };
}

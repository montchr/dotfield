{ ops, config, ... }:
let
  inherit (config.home) homeDirectory;
  inherit (ops) hosts;
  sshDir = "${homeDirectory}/.ssh";
  identityFile = "${sshDir}/id_ed25519";

in
{
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "gabbro".hostname = "${hosts.gabbro.ipv6.address}::1";
      "hierophant".hostname = "${hosts.hierophant.ipv6.address}::1";

      "platauc" = {
        hostname = "${hosts.platauc.ipv4.address}";
        identitiesOnly = true;
        # Overrides system remote builder config.
        inherit identityFile;
        user = "cdom";
      };

      "synoxyn" = {
        hostname = hosts.synoxyn.ipv4.address;
        port = 2367;
      };

      "atlantis" = {
        hostname = "atlantis.whatbox.ca";
        user = "syadasti";
      };

      "github.com" = {
        user = "git";
      };

      "eu.nixbuild.net" = {
        identityFile = "${sshDir}/id_ed25519_seadome_nixbuild_net";

        extraOptions = {
          PubkeyAcceptedKeyTypes = "ssh-ed25519";
        };
      };

      "*" = {
        inherit identityFile;

        identitiesOnly = true;
        forwardAgent = false;
        serverAliveInterval = 300;
        controlMaster = "auto";
        controlPersist = "10m";
      };
    };

    includes = [ "~/.config/ssh/config.local" ];
  };
}

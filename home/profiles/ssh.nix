{ flake, config, ... }:
let
  inherit (config.home) homeDirectory;
  inherit (flake.config.meta) hosts;

  sshDir = "${homeDirectory}/.ssh";
  identityFile = "${sshDir}/id_ed25519";
in
{
  programs.ssh = {
    enable = true;
    matchBlocks = {
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

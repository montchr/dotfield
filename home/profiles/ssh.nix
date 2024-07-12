{ ops, config, ... }:
let
  inherit (config.home) homeDirectory;
  inherit (ops) hosts;
  sshDir = "${homeDirectory}/.ssh";
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

      "platauc" = {
        hostname = "${hosts.platauc.ipv6.address}::1";
        identitiesOnly = true;
        # Overrides system remote builder config.
        identityFile = "${sshDir}/id_ed25519";
        user = "cdom";
      };

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
        identityFile = "${sshDir}/id_ed25519_seadome_nixbuild_net";
        identitiesOnly = true;
        extraOptions = {
          PubkeyAcceptedKeyTypes = "ssh-ed25519";
        };
      };
    };

    includes = [ "~/.config/ssh/config.local" ];
  };
}

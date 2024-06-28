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

    matchBlocks."gabbro".hostname = "${hosts.gabbro.ipv6.address}::1";
    matchBlocks."hierophant".hostname = "${hosts.hierophant.ipv6.address}::1";
    matchBlocks."moraine".hostname = "${hosts.moraine.ipv6.address}::1";

    matchBlocks."github.com" = {
      # inherit identityFile;
      # identitiesOnly = true;
      user = "git";
    };

    matchBlocks."eu.nixbuild.net" = {
      identityFile = "${homeDirectory}/.ssh/id_ed25519_seadome_nixbuild_net";
      identitiesOnly = true;
      extraOptions = {
        PubkeyAcceptedKeyTypes = "ssh-ed25519";
      };
    };

    includes = [ "~/.config/ssh/config.local" ];
  };
}

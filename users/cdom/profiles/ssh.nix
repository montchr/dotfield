{ ops, config, ... }:
let
  inherit (config.home) homeDirectory;
  inherit (ops) hosts;
in
{
  programs.ssh = {
    serverAliveInterval = 300;
    controlMaster = "auto";
    controlPersist = "10m";

    matchBlocks."gabbro".hostname = "${hosts.gabbro.ipv6.address}::1";
    matchBlocks."hierophant".hostname = "${hosts.hierophant.ipv6.address}::1";
    matchBlocks."moraine".hostname = "${hosts.moraine.ipv6.address}::1";

    matchBlocks."eu.nixbuild.net" = {
      identityFile = "${homeDirectory}/.ssh/id_ed25519_nixbuildnet";
      identitiesOnly = true;
    };

    # FIXME: use the ~/.ssh directory for its strict permissions enforcement
    includes = [ "~/.config/ssh/config.local" ];
  };
}

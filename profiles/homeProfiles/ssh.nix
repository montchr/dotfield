{
  ops,
  config,
  ...
}: let
  inherit (config.home) homeDirectory;
  inherit (ops.metadata) hosts;
in {
  programs.ssh = {
    enable = true;
    forwardAgent = false;
    serverAliveInterval = 300;

    matchBlocks."hierophant".hostname = "${hosts.hierophant.ipv6.address}::1";
    # TODO: why is this one different??? does not appear to be in the file.
    #       some difference between hetzner cloud and hetzner online?
    matchBlocks."moraine".hostname = hosts.moraine.ipv6.address;

    matchBlocks."github.com" = {
      # inherit identityFile;
      # identitiesOnly = true;
      user = "git";
    };

    matchBlocks."eu.nixbuild.net" = {
      identityFile = "${homeDirectory}/.ssh/id_ed25519_nixbuildnet";
      identitiesOnly = true;
    };

    includes = ["~/.config/ssh/config.local"];
  };
}

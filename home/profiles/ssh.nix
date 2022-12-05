{
  peers,
  config,
  ...
}: let
  inherit (config.home) homeDirectory;
  inherit (peers) hosts;
in {
  programs.ssh = {
    enable = true;
    forwardAgent = false;
    serverAliveInterval = 300;

    matchBlocks."hierophant".host = hosts.hierophant.ipv4.address;
    matchBlocks."github.com" = {
      # inherit identityFile;
      # identitiesOnly = true;
      user = "git";
    };
    matchBlocks."tsone".host = hosts.tsone.ipv4.address;

    matchBlocks."eu.nixbuild.net" = {
      identityFile = "${homeDirectory}/.ssh/id_ed25519_nixbuildnet";
      identitiesOnly = true;
    };

    includes = ["~/.config/ssh/config.local"];
  };
}

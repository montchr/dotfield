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

    matchBlocks."hierophant".hostname = hosts.hierophant.ipv4.address;
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

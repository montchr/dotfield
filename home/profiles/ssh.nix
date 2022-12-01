{peers, ...}: let
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
    includes = ["~/.config/ssh/config.local"];
  };
}

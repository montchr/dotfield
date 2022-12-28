{pkgs, ...}: {
  home.packages = [pkgs.lastpass-cli];
  programs.ssh.matchBlocks = {
    "kweb-prod-www" = {
      hostname = "67.225.164.90";
      port = 5623;
      user = "cdom";
    };
    "kweb-prod-db" = {
      hostname = "67.225.164.91";
      port = 522;
      user = "cdom";
    };
    "kweb-dev" = {
      hostname = "67.43.11.196";
      user = "cdom";
    };
  };
}

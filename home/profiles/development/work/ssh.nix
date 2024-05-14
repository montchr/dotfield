{
  programs.ssh.matchBlocks = {
    "kwg-ispr" = {
      user = "isprsite";
      hostname = "35.236.219.140";
      port = 24919;
    };
    "kwg-tutv" = {
      user = "templetv";
      hostname = "35.236.219.140";
      port = 38736;
    };
    "kwg-www" = {
      hostname = "67.225.164.90";
      port = 5623;
    };
    "kwg-db" = {
      hostname = "67.225.164.91";
      port = 522;
    };
    "kwg-dev" = {
      hostname = "67.43.11.196";
    };
  };
}

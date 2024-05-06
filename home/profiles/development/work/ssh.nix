{
  programs.ssh.matchBlocks = {
    "kw-www" = {
      hostname = "67.225.164.90";
      port = 5623;
    };
    "kw-db" = {
      hostname = "67.225.164.91";
      port = 522;
    };
    "kw-dev" = {
      hostname = "67.43.11.196";
    };
  };
}

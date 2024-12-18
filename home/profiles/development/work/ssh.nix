let
  mkKinstaHost = user: port: {
    inherit user port;
    hostname = "35.236.219.140";
  };
  mkKinstaHost2 = user: port: {
    inherit user port;
    hostname = "34.162.230.19";
  };
in
{
  programs.ssh.matchBlocks = {
    "isprsite-production" = mkKinstaHost "isprsite" 24919;

    "kleinforms-production" = mkKinstaHost "kleinforms" 49032;
    "kleinforms-staging" = mkKinstaHost "kleinforms" 30014;

    "phillyn-production" = mkKinstaHost "phillyn" 17385;
    "phillyn-staging" = mkKinstaHost "phillyn" 47991;

    "templetv-production" = mkKinstaHost "templetv" 38736;
    "templetv-staging" = mkKinstaHost "templetv" 59770;

    "seesaw-production" = mkKinstaHost2 "seesaw" 52637;

    "hostone" = {
      hostname = "67.225.164.90";
      port = 5623;
    };

    "kleinweb-db" = {
      hostname = "67.225.164.91";
      port = 522;
    };
  };
}

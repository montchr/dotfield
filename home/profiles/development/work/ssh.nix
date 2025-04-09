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

    "kleinsites-production" = mkKinstaHost "kleinsites" 60160;
    "kleinsites-staging" = mkKinstaHost "kleinsites" 17779;

    "logancenter-production" = mkKinstaHost "logancenter" 32428;
    "logancenter-staging" = mkKinstaHost "logancenter" 58819;

    "phillyn-production" = mkKinstaHost "phillyn" 17385;
    "phillyn-staging" = mkKinstaHost "phillyn" 47991;

    "telepresence-production" = mkKinstaHost "telepresence" 59096;
    # "telepresence-staging" = mkKinstaHost "telepresence" 59770;

    "templetv-production" = mkKinstaHost "templetv" 38736;
    "templetv-staging" = mkKinstaHost "templetv" 59770;

    "seesaw-production" = mkKinstaHost2 "seesaw" 30503;

    "hostone" = {
      hostname = "67.225.164.90";
      port = 5623;
    };
    "kleinsites-hostone" = {
      user = "kleinsites";
      hostname = "67.225.164.90";
      port = 5623;
    };

    "kleinweb-db" = {
      hostname = "67.225.164.91";
      port = 522;
    };
  };
}

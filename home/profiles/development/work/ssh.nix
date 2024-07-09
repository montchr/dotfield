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
    "kleinweb-ispr-prod" = mkKinstaHost "isprsite" 24919;

    "kleinweb-forms-prod" = mkKinstaHost "kleinforms" 46032;
    "kleinweb-forms-dev" = mkKinstaHost "kleinforms" 19154;

    "kleinweb-tutv-prod" = mkKinstaHost "templetv" 38736;
    "kleinweb-tutv-dev" = mkKinstaHost "templetv" 59770;

    "kleinweb-seesaw-prod" = mkKinstaHost2 "seesaw" 52637;

    "kleinweb-hostone" = {
      hostname = "67.225.164.90";
      port = 5623;
    };

    "kleinweb-db" = {
      hostname = "67.225.164.91";
      port = 522;
    };
  };
}

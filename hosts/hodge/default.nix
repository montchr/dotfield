{ config, inputs, suites, ... }:

{
  imports = with suites;
    personal
    ++ nixos
    ++ developer
    ++ [ ./configuration.nix ];

  networking.hostName = "hodge";
  dotfield.path = "/etc/hosts";

  nix.maxJobs = 4;
  nix.buildCores = 0;

  my = {
    username = "xtallos";
    email = "chris@cdom.io";
    website = "https://github.com/montchr/";
    hm.accounts.email.accounts.personal.primary = true;
  };
}

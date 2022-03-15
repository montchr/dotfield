{
  config,
  inputs,
  suites,
  profiles,
  ...
}: {
  imports = with suites;
    personal
    ++ nixos
    ++ developer
    # ++ profiles.emacs
    ++ [./configuration.nix];

  networking.hostName = "HodgePodge";
  dotfield.path = "/home/xtallos/dotfield";

  nix.maxJobs = 4;
  nix.buildCores = 4;

  my = {
    username = "xtallos";
    email = "chris@cdom.io";
    website = "https://github.com/montchr/";
    hm.accounts.email.accounts.personal.primary = true;
  };
}

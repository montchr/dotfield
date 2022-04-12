{
  config,
  inputs,
  suites,
  profiles,
  ...
}: {
  imports = with suites;
    personal
    ++ gui
    # ++ profiles.emacs
    ++ [./configuration.nix];

  networking.hostName = "HodgePodge";

  nix.maxJobs = 4;
  nix.buildCores = 4;

  my = {
    username = "xtallos";
    email = "chris@cdom.io";
    website = "https://github.com/montchr/";
  };
}

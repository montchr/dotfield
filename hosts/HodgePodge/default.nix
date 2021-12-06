{ config, inputs, ... }: {

  nixpkgs.system = "x86_64-darwin";
  # TODO: verify number of cores
  # nix.maxJobs = 4;
  nix.buildCores = 0;

  networking.hostName = "HodgePodge";
  networking.knownNetworkServices = [
    "Wi-Fi"
    "Bluetooth PAN"
    "iPhone USB"
    "Thunderbolt Bridge"
  ];

  my = {
    username = "cdom";
    email = "chris@cdom.io";
    website = "https://github.com/montchr/";
    hm.accounts.email.accounts.personal.primary = true;
  };

  homebrew = {
    casks = [
      "adobe-acrobat-reader"
      "audio-hijack"
      "soundsource"
      "tigervnc-viewer"
      "transmit"
    ];

    masApps = { "Paprika Recipe Manager 3" = 1303222628; };
  };
}

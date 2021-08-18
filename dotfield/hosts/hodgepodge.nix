{ config, inputs, ... }: {
  imports = [ ../modules/darwin ];

  my = {
    username = "cdom";
    email = "chris@cdom.io";
    website = "https://github.com/montchr/";
  };

  networking = {
    hostName = "HodgePodge";
    knownNetworkServices =
      [ "Wi-Fi" "Bluetooth PAN" "iPhone USB" "Thunderbolt Bridge" ];
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

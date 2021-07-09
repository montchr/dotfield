{ config, inputs, ... }: {
  imports = [ ../modules/darwin ];
  networking.hostName = "HodgePodge";

  my = {
    username = "cdom";
    email = "chris@cdom.io";
    website = "https://github.com/montchr/";
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

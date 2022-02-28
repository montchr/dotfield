{ config, inputs, suites, ... }:

{
  imports = with suites;
    darwin-gui
    ++ personal
    ++ developer;

  networking.hostName = "HodgePodge";

  nixpkgs.system = "x86_64-darwin";
  # TODO: verify number of cores
  # nix.maxJobs = 4;
  nix.buildCores = 0;

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

  # Explicitly disable special trackpad corner click behavior for HodgePodge due
  # to failing trackpad.
  #
  # On HodgePodge, the lower right corner is actually the *only* place where a
  # physical click will work (tapping will work anywhere, but it's not always
  # available). So if this default ever changes, we still want HodgePodge to
  # keep the setting disabled.
  system.defaults.NSGlobalDomain."com.apple.trackpad.trackpadCornerClickBehavior" = null;

  # Because of the machine's failing trackpad, tap-to-click is essential.
  system.defaults.NSGlobalDomain."com.apple.mouse.tapBehavior" = 1;
}

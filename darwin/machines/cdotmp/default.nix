{
  config,
  pkgs,
  lib,
  sharedProfiles,
  ...
}: {
  # FIXME: the profile file says this is "non-functional"... and i can't
  # remember why, but i believe me
  #
  # imports = with sharedProfiles; [users.chris];

  networking.hostName = "cdotmp";

  # FIXME: verify
  # $ networksetup -listallnetworkservices
  networking.knownNetworkServices = [
    "Bluetooth PAN"
    "Thunderbolt Bridge"
    "USB 10/100/1000 LAN"
    "Wi-Fi"
  ];
}

{
  config,
  pkgs,
  lib,
  suites,
  profiles,
  hmUsers,
  ...
}: {
  imports =
    (with suites; workstation)
    ++ (with profiles; [users.chris]);

  networking.hostName = "tmpln";

  # FIXME: verify
  # $ networksetup -listallnetworkservices
  networking.knownNetworkServices = [
    "Bluetooth PAN"
    "Thunderbolt Bridge"
    "USB 10/100/1000 LAN"
    "Wi-Fi"
  ];
}

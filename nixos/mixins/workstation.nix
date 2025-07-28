{
  flake,
  pkgs,
  config,
  ...
}:
{
  imports = [
    # ./desktop.nix

    # ../profiles/audio.nix
    # ../profiles/networking/avahi.nix
    # ../profiles/networking/protonvpn.nix

    # ../profiles/graphical/applications/1password.nix
    # ../profiles/graphical/applications/obs-studio.nix

    # ../profiles/hardware/android-devices/default.nix
    # ../profiles/hardware/bluetooth.nix
    # ../profiles/hardware/bluetooth-headset.nix
    # ../profiles/hardware/keyboard/default.nix
    # ../profiles/hardware/printers-scanners/default.nix
  ];

}

{ profiles }:
let
  audio = [
    profiles.audio
    profiles.bluetooth
    profiles.hardware.bluetooth-headset
  ];

  graphical = [
    profiles.desktop.common

    # FIXME: find a more appropriate "feature" to file systemd-boot under
    #        it is not universal -- there's also rEFInd but i haven't used that
    #        in a while. consider singularity.
    profiles.boot.systemd-boot
  ];

  # FIXME: why?
  # A tangible machine that is not a laptop.
  desktop =
    graphical
    ++ audio
    ++ tangible
    ++ [
      profiles.power
      profiles.networking.avahi
    ];

  # wlroots-based Wayland compositors
  wlroots = desktop ++ [
    profiles.desktop.kde-services
    profiles.networking.networkmanager
  ];

  # Machines I can physically touch.
  tangible = [ profiles.hardware.keyboard.default ];
in
{
  inherit
    audio
    desktop
    graphical
    tangible
    wlroots
    ;

  gnome = desktop ++ [
    profiles.desktop.gnome-desktop
    profiles.desktop.nixpkgs-wayland
    profiles.login.gdm
  ];

  server = [ profiles.server.acme ];

  # FIXME: pare these down, also they don't really have anything to do with 'webdev'
  webdev = [
    profiles.virtualisation.libvirt
    profiles.virtualisation.podman
    profiles.virtualisation.virt-manager
  ];

  workstation = desktop ++ [
    profiles.boot.systemd-boot
    profiles.location

    profiles.one-password
    profiles.bitwarden

    profiles.hardware.android-devices.common
    profiles.hardware.android-devices.supernote-a5x
    profiles.hardware.printers-scanners.common
    profiles.hardware.printers-scanners.epson-wf-3520
    profiles.hardware.yubikey
  ];
}

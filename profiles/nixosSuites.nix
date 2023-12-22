# FIXME: avoid these inherited loose variables! they will make builds take
# longer because their contents must be evaluated
{
  sharedProfiles,
  nixosProfiles,
}: let
  audio = [
    nixosProfiles.audio
    nixosProfiles.bluetooth
    nixosProfiles.hardware.bluetooth-headset
  ];

  graphical = [
    sharedProfiles.fonts.common
    sharedProfiles.fonts.fontconfig
    sharedProfiles.fonts.iosevka-variants

    nixosProfiles.desktop.common

    # TODO: this should be in some baseline profile since it's used repeatedly
    # and should generally be a default (is there any system where it is _not_
    # used?)
    nixosProfiles.boot.systemd-boot
  ];

  tangible = [
    nixosProfiles.hardware.keyboard
  ];

  # TODO: merge into desktop
  office = [
    nixosProfiles.desktop.zoom-us
    nixosProfiles.hardware.printers-scanners
  ];
in {
  inherit audio graphical office tangible;

  desktop =
    graphical
    ++ audio
    ++ office
    ++ tangible;

  gnome = [
    nixosProfiles.desktop.gnome-desktop
    nixosProfiles.login.gdm
  ];

  server = [
    nixosProfiles.server.acme
  ];

  # FIXME: pare these down, also they don't really have anything to do with 'webdev'
  webdev = [
    nixosProfiles.virtualisation.libvirt
    nixosProfiles.virtualisation.microvm-host
    nixosProfiles.virtualisation.podman
    nixosProfiles.virtualisation.vagrant
    nixosProfiles.virtualisation.virt-manager
  ];

  workstation = [
    sharedProfiles.secrets.default

    nixosProfiles.one-password
    nixosProfiles.bitwarden
    nixosProfiles.boot.systemd-boot
    nixosProfiles.hardware.yubikey
  ];
}

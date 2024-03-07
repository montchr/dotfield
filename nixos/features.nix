# FIXME: determine some approach to grouping/naming these collections of
# profiles in a way that i don't find them confusing or redundant a couple
# months later
{ sharedProfiles, nixosProfiles }:
let
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
    # used?) ... also, like, what does this have to do with GUIs?
    nixosProfiles.boot.systemd-boot
  ];

  tangible = [ nixosProfiles.hardware.keyboard ];
in
{
  inherit audio graphical tangible;

  desktop = graphical ++ audio ++ tangible;

  gnome = [
    nixosProfiles.desktop.gnome-desktop
    nixosProfiles.login.gdm
  ];

  server = [ nixosProfiles.server.acme ];

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
    nixosProfiles.desktop.zoom-us
    nixosProfiles.hardware.printers-scanners.common
    nixosProfiles.hardware.printers-scanners.epson-wf-3520
    nixosProfiles.hardware.yubikey
  ];
}

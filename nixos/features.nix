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

    # FIXME: find a more appropriate "feature" to file systemd-boot under
    #        it is not universal -- there's also rEFInd but i haven't used that
    #        in a while. consider singularity.
    nixosProfiles.boot.systemd-boot
    nixosProfiles.desktop.applications.default
    nixosProfiles.desktop.common
  ];

  desktop =
    graphical
    ++ audio
    ++ tangible
    ++ [
      nixosProfiles.power
      nixosProfiles.desktop.gnome-services
      nixosProfiles.networking.avahi
    ];

  wlroots = desktop ++ [
    nixosProfiles.desktop.kde-services
    nixosProfiles.networking.networkmanager
  ];

  tangible = [ nixosProfiles.hardware.keyboard ];
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
    nixosProfiles.desktop.gnome-desktop
    nixosProfiles.desktop.nixpkgs-wayland
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

  workstation = desktop ++ [
    sharedProfiles.secrets.default

    nixosProfiles.boot.systemd-boot
    nixosProfiles.location

    nixosProfiles.one-password
    nixosProfiles.bitwarden
    nixosProfiles.desktop.applications.zoom-us

    nixosProfiles.hardware.android-devices.common
    nixosProfiles.hardware.android-devices.supernote-a5x
    nixosProfiles.hardware.printers-scanners.common
    nixosProfiles.hardware.printers-scanners.epson-wf-3520
    nixosProfiles.hardware.yubikey
  ];
}

{
  sharedProfiles,
  nixosProfiles,
}: {
  graphical = [
    sharedProfiles.fonts.common
    sharedProfiles.fonts.fontconfig
    sharedProfiles.fonts.iosevka-variants

    nixosProfiles.boot.systemd-boot
    nixosProfiles.desktop.common
  ];

  gnome = [
    nixosProfiles.desktop.gnome-desktop
    nixosProfiles.login.gdm
  ];

  office = [
    nixosProfiles.desktop.zoom-us
    nixosProfiles.hardware.printers-scanners
  ];

  server = [
    nixosProfiles.server.acme
  ];

  tangible = [
    nixosProfiles.hardware.keyboard
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

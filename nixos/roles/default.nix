{
  sharedProfiles,
  nixosProfiles,
}: let
  graphical =
    [
      sharedProfiles.fonts.common
      sharedProfiles.fonts.fontconfig
      sharedProfiles.fonts.iosevka-variants
    ]
    ++ [
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
    nixosProfiles.networking.common
    nixosProfiles.networking.ssh-host
  ];

  tangible = [
    nixosProfiles.hardware.keyboard
    nixosProfiles.networking.common
  ];

  # FIXME: too many things! don't really need all of these all the time
  webdev = [
    nixosProfiles.virtualisation.libvirt
    nixosProfiles.virtualisation.microvm-host
    nixosProfiles.virtualisation.podman
    nixosProfiles.virtualisation.vagrant
    nixosProfiles.virtualisation.virt-manager
  ];

  workstation =
    [
      sharedProfiles.secrets
    ]
    ++ [
      nixosProfiles.one-password
      nixosProfiles.bitwarden
      nixosProfiles.boot.systemd-boot
      nixosProfiles.hardware.yubikey
      nixosProfiles.networking.common
      nixosProfiles.networking.ssh-host
    ];
in {
  inherit
    gnome
    graphical
    office
    server
    tangible
    webdev
    workstation
    ;
}

{
  sharedProfiles,
  nixosProfiles,
}: let
  graphical =
    (with sharedProfiles; [
      fonts.common
      fonts.fontconfig
      fonts.iosevka-variants
    ])
    ++ (with nixosProfiles; [
      boot.systemd-boot
      desktop.common
    ]);

  gnome = with nixosProfiles; [
    desktop.gnome-desktop
    login.gdm
  ];

  office = with nixosProfiles; [
    desktop.zoom-us
    hardware.printers-scanners
  ];

  server = with nixosProfiles; [
    networking.common
    networking.ssh-host
  ];

  tangible = with nixosProfiles; [
    hardware.keyboard
    networking.common
  ];

  webdev = with nixosProfiles; [
    virtualisation.libvirt
    virtualisation.microvm-host
    virtualisation.podman
    virtualisation.vagrant
    virtualisation.virt-manager
  ];

  workstation =
    (with sharedProfiles; [
      one-password
      secrets
    ])
    ++ (with nixosProfiles; [
      bitwarden
      boot.systemd-boot
      hardware.yubikey
      networking.common
      networking.ssh-host
    ]);
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

{
  sharedProfiles,
  nixosProfiles,
}: let
  graphical =
    (with sharedProfiles; [
      fonts.common
      fonts.fontconfig
      fonts.iosevka-variants
      one-password
    ])
    ++ (with nixosProfiles; [
      bitwarden
      boot.systemd-boot
      desktop.common
      desktop.zoom-us
    ]);

  gnome = with nixosProfiles; [
    desktop.gnome-desktop
    login.gdm
  ];

  server = with nixosProfiles; [
    networking.common
    networking.ssh-host
  ];

  tangible = with nixosProfiles; [
    hardware.keyboard
    hardware.printers-scanners
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
      secrets
    ])
    ++ (with nixosProfiles; [
      boot.systemd-boot
      hardware.yubikey
      networking.common
      networking.ssh-host
    ]);
in {
  inherit
    gnome
    graphical
    server
    tangible
    webdev
    workstation
    ;
}

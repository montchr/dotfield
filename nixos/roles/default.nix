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
      desktop.gnome-desktop
      desktop.zoom-us
    ]);

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
    # FIXME: virtualbox always causes a bunch of time-consuming rebuilds
    # virtualisation.virtualbox
  ];

  workstation =
    (with sharedProfiles; [
      one-password
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
    graphical
    server
    tangible
    webdev
    workstation
    ;
}

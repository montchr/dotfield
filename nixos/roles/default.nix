{
  sharedProfiles,
  nixosProfiles,
}: let
  graphical =
    (with sharedProfiles; [
      fonts.common
    ])
    ++ (with nixosProfiles; [
      boot.systemd-boot
      desktop.common
      desktop.gnome-desktop
      desktop.zoom-us
    ]);

  server =
    (with nixosProfiles; [
      networking.ssh-host
    ])
    ++ (with sharedProfiles; [
      networking.common
    ]);

  tangible =
    (with sharedProfiles; [
      networking.common
    ])
    ++ (with nixosProfiles; [
      hardware.keyboard
      hardware.printers-scanners
    ]);

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

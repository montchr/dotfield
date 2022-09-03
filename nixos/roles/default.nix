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
      desktop.video
      desktop.zoom-us
    ]);

  server =
    (with sharedProfiles; [
      networking.common
      networking.ssh-host
    ])
    ++ (with nixosProfiles; []);

  tangible =
    (with sharedProfiles; [
      networking.common
    ])
    ++ (with nixosProfiles; [
      hardware.audio
      hardware.bluetooth
      hardware.keyboard
      hardware.printers-scanners
      networking.wifi
    ]);

  webdev = with nixosProfiles; [
    virtualisation.libvirtd
    virtualisation.podman
    virtualisation.vagrant
    virtualisation.virtualbox
  ];

  workstation =
    (with sharedProfiles; [
      networking.ssh-host
      secrets
    ])
    ++ (with nixosProfiles; [
      boot.systemd-boot
      hardware.yubikey
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

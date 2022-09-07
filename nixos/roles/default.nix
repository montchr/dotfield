{
  collective,
  profiles,
}: let
  graphical =
    (with collective.profiles; [
      fonts.common
    ])
    ++ (with profiles; [
      boot.systemd-boot
      desktop.common
      desktop.gnome-desktop
      desktop.video
      desktop.zoom-us
    ]);

  server =
    (with (collective.profiles); [
      networking.common
      networking.ssh-host
    ])
    ++ (with profiles; []);

  tangible =
    (with (collective.profiles); [
      networking.common
    ])
    ++ (with profiles; [
      hardware.audio
      hardware.bluetooth
      hardware.keyboard
      hardware.printers-scanners
      networking.wifi
    ]);

  webdev = with profiles; [
    virtualisation.libvirtd
    virtualisation.podman
    virtualisation.vagrant
    virtualisation.virtualbox
  ];

  workstation =
    (with collective.profiles; [
      networking.ssh-host
      secrets
    ])
    ++ (with profiles; [
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

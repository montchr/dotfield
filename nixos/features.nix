let
  audio = [
    ./profiles/audio.nix
    ./profiles/bluetooth.nix
    ./profiles/hardware/bluetooth-headset.nix
  ];

  graphical = [
    ./profiles/desktop/common.nix

    # FIXME: find a more appropriate "feature" to file systemd-boot under
    #        it is not universal -- there's also rEFInd but i haven't used that
    #        in a while. consider singularity.
    ./profiles/boot/systemd-boot.nix
  ];

  # FIXME: why?
  # A tangible machine that is not a laptop.
  desktop =
    graphical
    ++ audio
    ++ tangible
    ++ [
      ./profiles/power.nix
      ./profiles/desktop/gnome-services.nix
      ./profiles/networking/avahi.nix
    ];

  # wlroots-based Wayland compositors
  wlroots = desktop ++ [
    ./profiles/desktop/kde-services.nix
    ./profiles/networking/networkmanager.nix
  ];

  # Machines I can physically touch.
  tangible = [ ./profiles/hardware/keyboard/default.nix ];
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
    ./profiles/desktop/gnome-desktop.nix
    ./profiles/desktop/nixpkgs-wayland.nix
    ./profiles/login/gdm.nix
  ];

  server = [ ./profiles/server/acme.nix ];

  # FIXME: pare these down, also they don't really have anything to do with 'webdev'
  webdev = [
    ./profiles/virtualisation/libvirt.nix
    ./profiles/virtualisation/podman.nix
    ./profiles/virtualisation/virt-manager.nix
  ];

  workstation = desktop ++ [
    ./profiles/boot/systemd-boot.nix
    ./profiles/location.nix

    ./profiles/one-password.nix
    ./profiles/bitwarden.nix

    ./profiles/hardware/android-devices/common.nix
    ./profiles/hardware/android-devices/supernote-a5x.nix
    ./profiles/hardware/printers-scanners/common.nix
    ./profiles/hardware/printers-scanners/epson-wf-3520.nix
    ./profiles/hardware/yubikey.nix
  ];
}

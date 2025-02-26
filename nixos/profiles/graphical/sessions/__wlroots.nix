{ pkgs, ... }:
{
  imports = [
    ../nixpkgs-wayland-overlay.nix
    ../common.nix
    ../kanshi.nix
  ];

  security.pam.services.swaylock = { };
  security.pam.services.waylock = { };

  # TODO: provide a default launcher
  environment.systemPackages = with pkgs; [
    gtk-layer-shell

    # essentials
    brightnessctl # display brightness
    grim # screenshot
    slurp # screenshots
    wev # input monitoring
    wl-clipboard # clipboard

    # swappables
    nautilus # gnome file manager

    # TODO: evaluate
    shikane # aims to be improvement over kanshi
    wluma # sensor-adaptive brightness adjustment daemon

  ];
}

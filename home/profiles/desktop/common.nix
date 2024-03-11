{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ../hardware/mouse.nix

    ./__clipboard.nix
    ./__xdg.nix

    ./gtk.nix
    ./qt.nix
  ];

  home.packages = [
    pkgs.gimp-with-plugins
    pkgs.mediainfo
    pkgs.ydotool # command-line automation tool
  ];

  fonts.fontconfig.enable = true;

  # gnome-keyring-daemon has issues as a user service
  # <https://github.com/NixOS/nixpkgs/issues/174099>
  # <https://github.com/nix-community/home-manager/issues/1454>
  services.gnome-keyring.enable = false;

  programs.zathura.enable = true;
}

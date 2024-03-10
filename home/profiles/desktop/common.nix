{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  inherit (config.home) homeDirectory;
  inherit (config.accounts.email) maildirBasePath;
in
{
  imports = [
    ../hardware/mouse.nix

    ./__clipboard.nix
    ./gtk.nix
    ./qt.nix
  ];

  home.packages = [
    pkgs.dconf2nix # <https://github.com/gvolpe/dconf2nix>
    pkgs.gimp-with-plugins
    pkgs.mediainfo
  ];

  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    extraConfig = {
      # TODO: somehow share this value with home-manager git-sync?
      XDG_PROJECTS_DIR = homeDirectory + "/Developer";
      XDG_MAIL_DIR = "${homeDirectory}/${maildirBasePath}";
    };
  };

  fonts.fontconfig.enable = true;

  # gnome-keyring-daemon has issues as a user service
  # <https://github.com/NixOS/nixpkgs/issues/174099>
  # <https://github.com/nix-community/home-manager/issues/1454>
  services.gnome-keyring.enable = false;

  programs.zathura.enable = true;

  # TODO
  # xdg.desktopEntries = ...
  # xdg.mime = ...
  # xdg.mimeApps = ...
}

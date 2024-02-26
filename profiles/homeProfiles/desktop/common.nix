{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  inherit (config.home) homeDirectory;
  inherit (config.accounts.email) maildirBasePath;
in {
  imports = [
    ./clipboard.nix
    ./input-devices.nix

    # ./dconf.settings.nix
  ];

  home.packages =
    [
      pkgs.gimp-with-plugins
      pkgs.mediainfo
    ]
    ++ lib.optionals isLinux [
      # TODO: only on gnome? or does gtk use it too?
      pkgs.dconf2nix #: <https://github.com/gvolpe/dconf2nix>
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

  # TODO: configure gtk settings
  # gtk.enable = true;
  # TODO: set these based on color scheme
  # gtk.gtk3.extraConfig.gtk-application-prefer-dark-theme = true;
  # gtk.gtk4.extraConfig.gtk-application-prefer-dark-theme = true;

  qt.enable = true;
  qt.platformTheme = "gnome";
  qt.style.package = pkgs.adwaita-qt;
  qt.style.name = "adwaita";

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

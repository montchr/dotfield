{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  inherit (config.home) homeDirectory;
  inherit (config.accounts.email) maildirBasePath;
in
  mkIf isLinux {
    # imports = [./dconf.settings.nix];

    home.packages = [
      # https://github.com/gvolpe/dconf2nix
      pkgs.dconf2nix
    ];

    xdg.userDirs = {
      enable = true;
      createDirectories = true;
      extraConfig = {
        XDG_PROJECTS_DIR = homeDirectory + "/Developer";
        XDG_MAIL_DIR = "$HOME/${maildirBasePath}";
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

    # https://github.com/NixOS/nixpkgs/issues/174099
    services.gnome-keyring.enable = false;

    programs.zathura.enable = true;

    # TODO
    # xdg.desktopEntries = ...
    # xdg.mime = ...
    # xdg.mimeApps = ...
  }

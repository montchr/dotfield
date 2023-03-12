hmArgs @ {
  inputs,
  config,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (config.dotfield.features) hasWayland;
  hasGnomeConnector = hmArgs.osConfig.services.gnome.gnome-browser-connector.enable or false;
in {
  imports = [./profiles.nix];
  programs.firefox = {
    enable = true;
    package =
      if isDarwin
      then pkgs.runCommand "firefox-0.0.0" {} "mkdir $out"
      else
        pkgs.firefox.override {
          wayland = hasWayland;
          cfg = {
            enableGnomeExtensions = hasGnomeConnector;
            enableTridactylNative = true;
          };
        };
  };
}

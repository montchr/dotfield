hmArgs @ {pkgs, ...}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  hasGnomeConnector = hmArgs.osConfig.services.gnome.gnome-browser-connector.enable or false;
in {
  imports = [
    ./profiles.nix
    ../os-specific/darwin/firefox-profile-hack.nix
  ];
  programs.firefox = {
    enable = true;
    package =
      if isDarwin
      then pkgs.runCommand "firefox-0.0.0" {} "mkdir $out"
      else
        pkgs.firefox.override {
          cfg = {
            enableGnomeExtensions = hasGnomeConnector;
            enableTridactylNative = true;
          };
        };
  };
}

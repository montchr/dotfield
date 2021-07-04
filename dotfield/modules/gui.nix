{ pkgs, lib, config, options, ... }:

let cfg = config.my.modules.gui;
in {
  options = with lib; {
    my.modules.gui = {
      enable = mkEnableOption ''
        Whether to enable gui module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable (mkMerge [
      (if (builtins.hasAttr "homebrew" options) then {
        homebrew = {
          taps = [ "homebrew/cask" "homebrew/cask-versions" ];
          masApps = {
            "Canary Mail" = 1236045954;
            "Xcode" = 497799835;
          };
          casks = [
            "1password"
            "1password-cli"
            "alfred"
            "appcleaner"
            "bartender"
            "basictex"
            "caprine"
            "corelocationcli"
            "dash"
            "db-browser-for-sqlite"
            "docker"
            "dropbox"
            "fantastical"
            "google-chrome"
            "google-drive"
            "imageoptim"
            "istat-menus"
            "kap"
            "karabiner-elements"
            "keyboard-maestro"
            "launchcontrol"
            "libreoffice"
            "marked"
            "muzzle"
            # FIXME: disabled due to SHA256 mismatch!
            # "plexamp"
            "qlcolorcode"
            "qlmarkdown"
            "qlstephen"
            "qlvideo"
            "quicklook-json"
            "quicklookase"
            "signal"
            "sketch"
            "slack"
            "steermouse"
            # "virtualbox"
            "visual-studio-code"
            "vlc"
            "webpquicklook"
            "xquartz"
            "zoom"
          ];
        };
      } else {
        my.user = { packages = with pkgs; [ signal-desktop ]; };
      })
    ]);
}

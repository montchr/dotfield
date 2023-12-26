# Adapted from <https://github.com/nix-community/home-manager/blob/master/modules/services/plex-mpv-shim.nix>
{
  config,
  lib,
  pkgs,
  ...
}: let
  jsonFormat = pkgs.formats.json {};
  cfg = config.services.jellyfin-mpv-shim;
in {
  meta.maintainers = with lib.maintainers; [montchr];

  options = {
    services.jellyfin-mpv-shim = {
      enable = lib.mkEnableOption "Jellyfin mpv shim";

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.jellyfin-mpv-shim;
        defaultText = lib.literalExpression "pkgs.jellyfin-mpv-shim";
        description = "The package to use for the Jellyfin mpv shim.";
      };

      settings = lib.mkOption {
        type = jsonFormat.type;
        default = {
          check_updates = false;
          notify_updates = false;
        };
        defaultText = lib.literalExpression ''
          {
            check_updates = false;
            notify_updates = false;
          }
        '';
        example = lib.literalExpression ''
          {
            always_transcode = false;
            enable_osc = false;
            fullscreen = false;
            subtitle_size = 150;
            transcode_4k = true;
          }
        '';
        description = ''
          Configuration written to
          {file}`$XDG_CONFIG_HOME/jellyfin-mpv-shim/config.json`. See
          <https://github.com/jellyfin/jellyfin-mpv-shim#configuration>
          for the configuration documentation.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "services.jellyfin-mpv-shim" pkgs
        lib.platforms.linux)
    ];

    home.packages = [cfg.package];

    xdg.configFile."jellyfin-mpv-shim/conf.json" = lib.mkIf (cfg.settings != {}) {
      source = jsonFormat.generate "conf.json" cfg.settings;
    };

    systemd.user.services.jellyfin-mpv-shim = {
      Unit = {
        Description = "Jellyfin mpv shim";
        After = ["graphical-session-pre.target"];
        PartOf = ["graphical-session.target"];
      };

      Service = {ExecStart = "${cfg.package}/bin/jellyfin-mpv-shim";};

      Install = {WantedBy = ["graphical-session.target"];};
    };
  };
}

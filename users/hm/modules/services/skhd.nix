{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.services.skhd;
in {
  options = {
    services.skhd.enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable the skhd hotkey daemon.";
    };

    services.skhd.package = mkOption {
      type = types.package;
      default = pkgs.skhd;
      description = "Package providing skhd.";
    };

    services.skhd.config = mkOption {
      type = types.lines;
      default = "";
      example = "alt + shift - r   :   yabai quit";
      description = "Contents of <filename>skhdrc</filename>.";
    };
  };

  config = mkIf cfg.enable {
    # skhd must be available to shells for keypress simulation functionality,
    # e.g. exiting out of modes after running a script.
    home.packages = [cfg.package];

    xdg.configFile."skhd/skhdrc" = {
      text = cfg.config;
      onChange = "${cfg.package}/bin/skhd -r";
    };

    launchd.agents.skhd = {
      enable = lib.mkDefault true;
      # path = [ config.environment.systemPath ];
      config = {
        ProgramArguments =
          ["${cfg.package}/bin/skhd"]
          ++ optionals (cfg.config != "") ["-c" "${config.xdg.configHome}/skhd/skhdrc"];
        KeepAlive = true;
        ProcessType = "Interactive";

        # TODO: make these paths configurable
        StandardOutPath = "${config.xdg.cacheHome}/skhd.out.log";
        StandardErrorPath = "${config.xdg.cacheHome}/skhd.err.log";
      };
    };
  };
}

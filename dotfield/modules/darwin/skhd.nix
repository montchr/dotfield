{ config, lib, options, pkgs, ... }:

let
  configDir = "${config.dotfield.flkConfigDir}/skhd";
in
{
  options = with lib; {
    my.modules.skhd = {
      enable = mkEnableOption ''
        Whether to enable skhd module
      '';
    };
  };

  config = {
    services.skhd = { enable = true; };

    # The =skhd= program must be available to shells for keypress simulation
    # functionality, which is essential for exiting out of modes after running a
    # script, for example.
    my.user.packages = with config.services; [ skhd.package ];

    my.hm.configFile."skhd/skhdrc".source = "${configDir}/skhdrc";

    launchd.user.agents.skhd = {
      serviceConfig = {
        StandardOutPath = "${config.my.xdgPaths.cache}/skhd.out.log";
        StandardErrorPath = "${config.my.xdgPaths.cache}/skhd.err.log";
      };
    };
  };
}

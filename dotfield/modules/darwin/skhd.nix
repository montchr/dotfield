{ config, lib, options, pkgs, ... }:

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
    my.user.packages = with config.services; [ skhd.package ];

    launchd.user.agents.skhd = {
      serviceConfig = {
        StandardOutPath = "${config.my.xdgPaths.cache}/skhd.out.log";
        StandardErrorPath = "${config.my.xdgPaths.cache}/skhd.err.log";
      };
    };
  };
}

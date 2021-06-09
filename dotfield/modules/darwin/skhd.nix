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
    my.user.packages = with pkgs; [ skhd ];
    services.skhd = { enable = true; };

    launchd.user.agents.skhd = {
      serviceConfig = {
        StandardOutPath = "${config.my.xdgPaths.cache}/skhd.out.log";
        StandardErrorPath = "${config.my.xdgPaths.cache}/skhd.err.log";
      };
    };
  };
}

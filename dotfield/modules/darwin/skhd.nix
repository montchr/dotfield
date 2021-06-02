{ config, lib, options, ... }:

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

    launchd.user.agents.skhd = {
      path = [ config.my.xdgPaths.bin ];
      serviceConfig = {
        StandardOutPath = "${config.my.xdgPaths.cache}/skhd.out.log";
        StandardErrorPath = "${config.my.xdgPaths.cache}/skhd.err.log";
      };
    };
  };
}

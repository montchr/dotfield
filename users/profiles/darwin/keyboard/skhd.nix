{ config, lib, options, pkgs, ... }:
let
  inherit (config) my;

  configDir = "${config.dotfield.configDir}/skhd";
  skhdPkg = config.services.skhd.package;
in
{
  services.skhd.enable = true;

  # The =skhd= program must be available to shells for keypress simulation
  # functionality, which is essential for exiting out of modes after running a
  # script, for example.
  my.user.packages = [ skhdPkg ];

  my.hm.xdg.configFile."skhd/skhdrc" = {
    source = "${configDir}/skhdrc";
    onChange = "${skhdPkg}/bin/skhd -r";
  };

  launchd.user.agents.skhd = {
    serviceConfig = {
      StandardOutPath = "${my.xdg.cache}/skhd.out.log";
      StandardErrorPath = "${my.xdg.cache}/skhd.err.log";
    };
  };
}

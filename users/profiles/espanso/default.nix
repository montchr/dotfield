{ config, lib, pkgs, ... }:

let
  hmConfig = config.home-manager.users.${config.my.username};

  configSrc = "${config.dotfield.configDir}/espanso";
  matchesSrc = "${configSrc}/match";
  matchesDest = "${hmConfig.xdg.configHome}/espanso/match";

  mkMatchesSecret = name: {
    "espanso-${name}.yml" = {
      file = "${matchesSrc}/${name}.yml.age";
      path = "${matchesDest}/${name}.yml";
      owner = hmConfig.home.username;
    };
  };
in

{

  my.hm.xdg.configFile."espanso" = {
    source = configSrc;
    recursive = true;
  };

  age.secrets = lib.mkMerge (builtins.map mkMatchesSecret [
    "personal"
    "work"
  ]);

}

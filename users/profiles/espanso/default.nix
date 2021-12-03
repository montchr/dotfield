{ config, lib, pkgs, ... }:

let
  configSrc = "${config.dotfield.configDir}/espanso";
  matchesSrc = "${configSrc}/match";
  matchesDest = "${config.my.xdg.config}/espanso/match";

  mkMatchesSecret = name: {
    "espanso-${name}" = {
      file = "${matchesSrc}/${name}.yml.age";
      path = "${matchesDest}/${name}.yml";
      owner = config.my.user.name;
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

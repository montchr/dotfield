{
  config,
  osConfig,
  lib,
  pkgs,
  ...
}: let
  configSrc = "${osConfig.dotfield.configDir}/espanso";
  matchesSrc = "${configSrc}/match";
  matchesDest = "${config.xdg.configHome}/espanso/match";

  mkMatchesSecret = name: {
    "espanso-${name}.yml" = {
      file = "${matchesSrc}/${name}.yml.age";
      path = "${matchesDest}/${name}.yml";
      owner = config.home.username;
    };
  };
in {
  xdg.configFile."espanso" = {
    source = configSrc;
    recursive = true;
  };

  age.secrets = lib.mkMerge (builtins.map mkMatchesSecret [
    "personal"
    "work"
  ]);
}

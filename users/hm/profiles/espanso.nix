{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  configSrc = ../../../config/espanso;
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

  # FIXME: use hm age module, needs identityPaths
  # age.secrets = lib.mkMerge (builtins.map mkMatchesSecret [
  #   "personal"
  #   "work"
  # ]);
}

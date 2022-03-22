{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (inputs.gitignore.lib) gitignoreSource;
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
    source = gitignoreSource configSrc;
    recursive = true;
  };

  age.secrets = lib.mkMerge (builtins.map mkMatchesSecret [
    "personal"
    "work"
  ]);
}

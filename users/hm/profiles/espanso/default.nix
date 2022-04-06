{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  mkMatchesSecret = name: {
    "espanso-${name}.yml" = {
      file = ./. + "/${name}.yml.age";
      path = "${config.xdg.configHome}/espanso/match/${name}.yml";
      owner = config.home.username;
    };
  };
in {
  xdg.configFile."espanso" = {
    source = "${pkgs.dotfield-config}/espanso";
    recursive = true;
  };

  # FIXME: still broken!
  # age.secrets = lib.mkMerge (builtins.map mkMatchesSecret [
  #   "personal"
  #   "work"
  # ]);
}

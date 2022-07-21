{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (config.lib) dotfield;
  inherit (config.home) username;
  inherit
    (config.xdg)
    configHome
    dataHome
    stateHome
    ;

  profilePath = "emacs/profiles/crafted";
in {
  imports = [./common.nix];

  home.sessionVariables."CRAFTED_EMACS_HOME" =
    "${dotfield.fsPath}/users/${username}/config/${profilePath}";
}

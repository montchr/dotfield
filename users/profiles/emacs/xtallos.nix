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

  hmLib = config.lib;

  profilePath = "emacs/profiles/xtallos";
in {
  imports = [./common.nix];

  xdg.configFile."emacs".source = hmLib.file.mkOutOfStoreSymlink
    "${dotfield.fsPath}/users/${username}/config/${profilePath}";
}

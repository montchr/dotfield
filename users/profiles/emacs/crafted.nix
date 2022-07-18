{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (config.home) username;
  inherit
    (config.xdg)
    configHome
    dataHome
    stateHome
    ;

  hmLib = config.lib;

  profilePath = "emacs/profiles/crafted";
in {
  imports = [./common.nix];

  xdg.configFile."emacs".source = inputs.crafted-emacs.outPath;
  xdg.configFile."crafted-emacs".source =
    hmLib.file.mkOutOfStoreSymlink
    "${configHome}/dotfield/users/${username}/config/${profilePath}";
}

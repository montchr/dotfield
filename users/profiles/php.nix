{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  inherit (pkgs) system writeShellScriptBin;
  inherit (inputs.phps.packages.${system}) php81;
  inherit (pkgs.lib.our) dotfieldPath;
in {
  home.packages = with pkgs; [
    php81
    php81.packages.composer
  ];
  home.sessionPath = [
    # TODO: prepend not append
    "${dotfieldPath}/.composer/bin"
  ];
  home.sessionVariables = {
    COMPOSER_HOME = "$XDG_STATE_HOME/composer";
  };
}

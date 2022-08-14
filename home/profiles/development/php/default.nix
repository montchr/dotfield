{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (config.lib) dotfield;
in {
  home.packages = with pkgs; [
    php80
    php80Packages.composer
  ];

  # TODO: use composer2nix or something?
  home.sessionPath = [
    # FIXME: prepend, not append
    "${dotfield.fsPath}/.composer/bin"
  ];

  home.sessionVariables = {
    COMPOSER_HOME = "$XDG_STATE_HOME/composer";
  };
}

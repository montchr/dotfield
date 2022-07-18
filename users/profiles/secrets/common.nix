{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.lib) dotfield;
in {
  home.sessionVariables.AGENIX_ROOT = dotfield.fsPath;
}

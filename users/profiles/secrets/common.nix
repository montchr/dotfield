{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.lib.dotfield.whoami) pgpPublicKey;

  dotfieldPath = "${config.xdg.configHome}/dotfield";
in {
  home.sessionVariables.AGENIX_ROOT = dotfieldPath;
}

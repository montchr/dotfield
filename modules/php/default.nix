{ config, pkgs, lib, ... }:
with lib;
let
  inherit (pkgs) php74 php74Packages writeShellScriptBin;
  inherit (php74Packages) composer;

  dotfieldPath = config.dotfield.path;

  cfg = config.my.modules.php;
  configDir = "${config.dotfield.configDir}/php";

  composer2nixPkgs = pkgs // { php = php74; phpPackages = php74Packages; };
  common = import ./common { pkgs = composer2nixPkgs; };
  phpactor = import ./phpactor { pkgs = composer2nixPkgs; };

in
{
  options = { my.modules.php = { enable = mkEnableOption false; }; };

  config = mkIf cfg.enable {
    environment.variables = {
      PATH = [
        "${dotfieldPath}/.composer/bin"
        "${common.outPath}/vendor/bin"
        "$PATH"
      ];
    };

    my.env = {
      COMPOSER_HOME = "${config.my.xdg.config}/composer";
    };

    my.user.packages = [
      php74
      # FIXME: collides with composer bin from phpactor!
      composer

      # TODO: disabled for no reason in particular, just trying to keep moving
      common

      # FIXME: disabled because one of its composer dependencies adds a
      # `jsonlint` executable into the environment, which breaks
      # json-language-server in emacs
      # phpactor

      # Get the current `php` executable's version number.
      (writeShellScriptBin "php-version" "php -v | awk '/^PHP/{print $2}'")
    ];

  };
}

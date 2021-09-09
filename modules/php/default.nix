{ config, pkgs, lib, ... }:
with lib;
let
  inherit (pkgs) php74 php74Packages writeShellScriptBin;
  inherit (php74Packages) composer;

  cfg = config.my.modules.php;
  configDir = "${config.dotfield.configDir}/php";

in {
  options = { my.modules.php = { enable = mkEnableOption false; }; };

  config = mkIf cfg.enable {
    my.env = {
      PATH = [
        "$DOTFIELD_DIR/.composer/bin"
        # "$XDG_DATA_HOME/composer/vendor/bin"
        # "$XDG_DATA_HOME/phpactor/vendor/bin"
      ];
    };

    my.user.packages = let
      composer2nixPkgs = pkgs // { php = php74; phpPackages = php74Packages; };
      common = import ./common { pkgs = composer2nixPkgs; };
      phpactor = import ./phpactor { pkgs = composer2nixPkgs; };
    in [
      php74
      # FIXME: collides with composer bin from phpactor!
      # composer
      common
      phpactor

      # Get the current `php` executable's version number.
      (writeShellScriptBin "php-version" "php -v | awk '/^PHP/{print $2}'")
    ];

  };
}

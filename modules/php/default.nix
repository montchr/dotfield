{ config, pkgs, lib, ... }:
with lib;
let
  inherit (pkgs) callPackage php74 php74Packages writeShellScriptBin;
  inherit (php74Packages) composer;

  cfg = config.my.modules.php;

  composerEnv = import ./composer-env.nix {
    inherit (pkgs) stdenv lib writeTextFile fetchurl php unzip phpPackages;
  };

in {
  options = { my.modules.php = { enable = mkEnableOption false; }; };

  config = mkIf cfg.enable {
    my.user.packages = let
    in [
      php74
      composer

      # Get the current `php` executable's version number.
      (writeShellScriptBin "php-version" "php -v | awk '/^PHP/{print $2}'")
    ];

  };
}

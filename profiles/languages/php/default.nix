{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit
    (pkgs)
    php74
    php74Packages
    writeShellScriptBin
    ;
  inherit (php74Packages) composer;

  dotfieldPath = config.dotfield.path;
  configDir = "${config.dotfield.configDir}/php";

  composer2nixPkgs =
    pkgs
    // {
      php = php74;
      phpPackages = php74Packages;
    };
  common = import ./common {pkgs = composer2nixPkgs;};
  phpactor = import ./phpactor {pkgs = composer2nixPkgs;};
in {
  environment.systemPackages = [
    php74
    common
    composer

    # FIXME: disabled because one of its composer dependencies adds a
    # `jsonlint` executable into the environment, which breaks
    # json-language-server in emacs
    # FIXME: collides with php74packages.composer!
    # phpactor

    # Get the current `php` executable's version number.
    (writeShellScriptBin "php-version" "php -v | awk '/^PHP/{print $2}'")
  ];

  environment.variables = {
    PATH = [
      "${dotfieldPath}/.composer/bin"
      "${common.outPath}/vendor/bin"
      "$PATH"
    ];
  };
}

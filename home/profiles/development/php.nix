{ flake, pkgs, ... }:
{
  home.packages = [
    # FIXME: conflicts with wp-cli php.ini
    #        wp-cli should probably be an alias for phpPackages.wp-cli ? but i
    #        don't think that is the case.
    # pkgs.php
    # pkgs.phpPackages.composer

    # unfree, but the alternatives are in a sad state
    pkgs.nodePackages.intelephense

    pkgs.phpactor
    pkgs.phpPackages.php-cs-fixer
    pkgs.phpPackages.psysh
    pkgs.wp-cli

    # Provides DAP connection to Xdebug for editor support
    flake.packages.vscode-php-debug
  ];

  home.sessionVariables = {
    COMPOSER_HOME = "$XDG_STATE_HOME/composer";
  };

  home.shellAliases."c" = "composer";
  home.shellAliases."cl" = "composer lint";
  home.shellAliases."cf" = "composer fix";
}

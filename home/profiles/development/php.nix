{ flake, pkgs, ... }:
{
  home.packages = [
    pkgs.phpactor
    pkgs.wp-cli

    # Provides DAP connection to Xdebug for editor support
    pkgs.vscode-extensions.xdebug.php-debug

    # Requires further setup: <https://wiki.nixos.org/wiki/Jetbrains_Tools#JetBrains_Toolbox>
    pkgs.jetbrains-toolbox
  ];

  home.sessionVariables = {
    COMPOSER_HOME = "$XDG_STATE_HOME/composer";
  };
}

{pkgs, ...}: {
  home.packages = with pkgs; [
    php
    phpPackages.composer
  ];

  home.sessionVariables = {
    COMPOSER_HOME = "$XDG_STATE_HOME/composer";
  };

  home.shellAliases."c" = "composer";
  home.shellAliases."cl" = "composer lint";
  home.shellAliases."cf" = "composer fix";
}

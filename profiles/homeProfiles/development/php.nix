{pkgs, ...}: {
  home.packages = with pkgs; [
    php
    phpPackages.composer
  ];

  home.sessionVariables = {
    COMPOSER_HOME = "$XDG_STATE_HOME/composer";
  };
}

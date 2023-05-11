{pkgs, ...}: {
  home.packages = with pkgs; [
    php80
    php80Packages.composer
  ];

  home.sessionVariables = {
    COMPOSER_HOME = "$XDG_STATE_HOME/composer";
  };
}

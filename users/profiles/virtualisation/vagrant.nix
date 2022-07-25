{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [../ruby.nix];

  home.packages = with pkgs; [vagrant];

  home.sessionVariables = {
    VAGRANT_ALIAS_FILE = "$XDG_DATA_HOME/vagrant/aliases";
    VAGRANT_HOME = "$XDG_DATA_HOME/vagrant";
  };
}

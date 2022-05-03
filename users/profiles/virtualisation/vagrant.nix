{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [../languages/ruby.nix];

  home.sessionVariables = {
    VAGRANT_ALIAS_FILE = "$XDG_DATA_HOME/vagrant/aliases";
    VAGRANT_HOME = "$XDG_DATA_HOME/vagrant";
  };
}

{ inputs, pkgs, ... }:
{
  home-manager.users."cdom".home.packages = [ pkgs.wp-cli ];
}

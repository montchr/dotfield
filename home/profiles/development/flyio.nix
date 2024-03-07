{ pkgs, ... }:
{
  home.packages = with pkgs; [ flyctl ];
  home.shellAliases.fly = "flyctl";
}

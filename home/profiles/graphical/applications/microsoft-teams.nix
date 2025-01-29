{ pkgs, ... }:
{
  home.packages = [ pkgs.teams-for-linux ];
  wayland.windowManager.sway.config.startup = [
    {
      command = "teams-for-linux";
    }
  ];
}

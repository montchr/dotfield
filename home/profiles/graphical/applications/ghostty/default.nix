{
  programs.ghostty = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
    installBatSyntax = true;
    settings = {
      window-decoration = false;
    };
  };

  programs.fuzzel.settings.main.terminal = "ghostty";
  wayland.windowManager.sway.config.terminal = "ghostty";
  dconf.settings."org/cinnamon/desktop/applications/terminal" = {
    exec = "ghostty";
  };
}

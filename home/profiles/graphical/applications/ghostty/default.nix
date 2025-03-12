{
  programs.ghostty = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
    installBatSyntax = true;
    settings = {
      confirm-close-surface = false;
      window-decoration = false;
    };
  };

  programs.fuzzel.settings.main.terminal = "ghostty";
  wayland.windowManager.sway.config.terminal = "ghostty";
  dconf.settings."org/cinnamon/desktop/applications/terminal" = {
    exec = "ghostty";
  };
}

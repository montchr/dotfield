{
  programs.ghostty = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
    installBatSyntax = true;
    settings = {
      font-size = 11;
      theme = "dark:catppuccin-frappe,light:catppuccin-latte";
      window-decoration = false;
    };
  };
  programs.fuzzel.settings.main.terminal = "ghostty";
  wayland.windowManager.sway.config.terminal = "ghostty";
}

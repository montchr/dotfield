{
  programs.ghostty = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
    installBatSyntax = true;
    settings = {
      font-size = 13;
      theme = "dark:catppuccin-frappe,light:catppuccin-latte";
      window-decoration = false;
    };
  };
}

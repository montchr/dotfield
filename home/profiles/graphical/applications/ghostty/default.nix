{
  programs.ghostty = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
    installBatSyntax = true;
    settings = {
      confirm-close-surface = false;
      # Incompatible with `window-inherit-working-directory`.
      # <https://github.com/ghostty-org/ghostty/discussions/4123#discussioncomment-13433453>
      gtk-single-instance = false;
      window-decoration = false;
      window-inherit-working-directory = true;
      working-directory = "inherit";
    };
  };

}

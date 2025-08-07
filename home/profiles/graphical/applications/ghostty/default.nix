{
  programs.ghostty = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
    installBatSyntax = true;
    settings = {
      confirm-close-surface = false;
      # Ensure a predictable font width.
      font-family = "Iosevka Term";
      # Incompatible with `window-inherit-working-directory`.
      # <https://github.com/ghostty-org/ghostty/discussions/4123#discussioncomment-13433453>
      gtk-single-instance = false;
      window-decoration = false;
      window-inherit-working-directory = true;
      working-directory = "inherit";
    };
  };

}

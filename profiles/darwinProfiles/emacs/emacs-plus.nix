{
  homebrew.taps = ["d12frosted/emacs-plus"];
  homebrew.brews = [
    # Required to make emacs 29+ build with tree-sitter support.
    # <https://github.com/d12frosted/homebrew-emacs-plus/issues/527>
    "tree-sitter"
    {
      name = "emacs-plus@29";
      args = [
        "with-debug"
        "with-imagemagick"
        "with-mailutils"
        "with-native-comp"
        "with-modern-papirus-icon"
        "with-poll"
      ];
      start_service = true;
      restart_service = true;
      link = true;
    }
  ];
}

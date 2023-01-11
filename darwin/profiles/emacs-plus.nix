# FIXME: needs tree-sitter installed via brew...
{
  homebrew.taps = ["d12frosted/emacs-plus"];
  homebrew.brews = [
    {
      name = "emacs-plus@30";
      args = [
        "with-debug"
        "with-imagemagick"
        "with-mailutils"
        "with-native-comp"
        "with-modern-papirus-icon"
      ];
      start_service = true;
      restart_service = true;
      link = true;
    }
  ];
}

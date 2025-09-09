let
  desktopEntryNames = {
    ghostty = "com.mitchellh.ghostty";
    zathura = "org.pwmt.zathura-pdf-mupdf";
  };

  nameFor = app: (desktopEntryNames.${app} or app) + ".desktop";
in
{
  flake.lib.mimeapps = {
    inherit desktopEntryNames nameFor;
  };
}

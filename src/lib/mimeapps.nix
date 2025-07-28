let
  desktopEntryNames = {
    ghostty = "com.mitchellh.ghostty";
  };

  nameFor = app: (desktopEntryNames.${app} or app) + ".desktop";
in
{
  flake.lib.mimeapps = { inherit desktopEntryNames nameFor; };
}

{ self, ... }:
{
  desktopEntryNames = {
    ghostty = "com.mitchellh.ghostty";
  };

  nameFor = app: (self.desktopEntryNames.${app} or app) + ".desktop";
}

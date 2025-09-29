{ self, ... }:
{
  users.cdom.aspects.graphical.home =
    { config, ... }:
    let
      inherit (self.lib) mimeapps mimetypes;
    in
    {
      xdg.mimeApps.enable = true;
      xdg.mimeApps.defaultApplications =
        (mimetypes.genAssoc mimetypes.audio (mimeapps.nameFor "mpv"))
        // (mimetypes.genAssoc mimetypes.text (mimeapps.nameFor "emacsclient"))
        // (mimetypes.genAssoc mimetypes.video (mimeapps.nameFor "mpv"))
        // (mimetypes.genAssoc mimetypes.webpage (mimeapps.nameFor "firefox"))
        // {
          "application/pdf" = [ (mimeapps.nameFor "zathura") ];
          "inode/directory" = [ (mimeapps.nameFor "nautilus") ];
          "x-scheme-handler/terminal" = [ (mimeapps.nameFor "ghostty") ];
        };
    };
}

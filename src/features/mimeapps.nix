{ self, ... }:
let
  inherit (self.lib) mimeapps mimetypes;
in
{
  dotfield.modules.graphical.home =
    { config, pkgs, ... }:
    let
      prefs = self.dotfield.meta.users.${config.home.username}.preferences;
    in
    {
      xdg.mimeApps.enable = true;
      xdg.mimeApps.defaultApplications =
        (mimetypes.genAssoc mimetypes.audio (mimeapps.nameFor prefs.audio-player))
        // (mimetypes.genAssoc mimetypes.text (mimeapps.nameFor prefs.editor))
        // (mimetypes.genAssoc mimetypes.video (mimeapps.nameFor prefs.video-player))
        // (mimetypes.genAssoc mimetypes.webpage (mimeapps.nameFor prefs.web-browser))
        // {
          "inode/directory" = [ (mimeapps.nameFor prefs.file-manager) ];
          "x-scheme-handler/terminal" = [ (mimeapps.nameFor prefs.term) ];
        };
    };
}

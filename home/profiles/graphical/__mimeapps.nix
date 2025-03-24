{
  flake,
  config,
  pkgs,
  ...
}:
let
  inherit (flake.self.lib) mimeapps mimetypes;
  prefs = import "${flake.self}/users/${config.home.username}/preferences.nix" {
    inherit pkgs;
  };
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
}

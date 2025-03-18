{
  flake,
  config,
  ...
}:
let
  inherit (flake.lib) mimetypes;
  prefs = import "${flake.self}/users/${config.home.username}/preferences.nix" {
    inherit pkgs;
  };
in
{
  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications =
    (mimetypes.genAssoc mimetypes.audio "${prefs.audio-player}.desktop")
    // (mimetypes.genAssoc mimetypes.text "${prefs.editor}.desktop")
    // (mimetypes.genAssoc mimetypes.video "${prefs.video-player}.desktop")
    // (mimetypes.genAssoc mimetypes.webpage "${prefs.web-browser}.desktop")
    // {
      "inode/directory" = [ "${prefs.file-manager}.desktop" ];
      "x-scheme-handler/terminal" = [ "${prefs.term}.desktop" ];
    };
}

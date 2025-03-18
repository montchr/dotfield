{ flake, lib, ... }:
let
  inherit (flake.lib) mimetypes;
in
{
  xdg.mimeApps.defaultApplications =
    (mimetypes.genAssoc mimetypes.archive "org.gnome.FileRoller.desktop")
    // (mimetypes.genAssoc mimetypes.image "org.gnome.Loupe.desktop");
}

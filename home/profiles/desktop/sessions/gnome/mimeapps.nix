{ lib, ... }:
{
  xdg.mimeApps.defaultApplications =
    let
      imageTypes = [
        "image/gif"
        "image/jpeg"
        "image/png"
        # "image/svg+xml"
        "image/tiff"
        "image/webp"
      ];
    in
    lib.genAttrs imageTypes (_: [ "org.gnome.Loupe.desktop" ]);
}

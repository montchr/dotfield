# <https://orgmode.org/worg/org-contrib/org-protocol.html#orgbfd8e10>
{ pkgs, ... }:
let
  name = "Org-Protocol";
  mimeType = "x-scheme-handler/org-protocol";
in
{
  home.packages = [
    (pkgs.makeDesktopItem {
      inherit name;
      mimeTypes = [ mimeType ];
      desktopName = name;
      keywords = [
        "Org-Mode"
        "Emacs"
      ];
      comment = "Intercept calls from emacsclient to trigger custom actions";
      startupWMClass = "Emacs";
      exec = "emacsclient -- %u";
      terminal = false;
      icon = "emacs";
      categories = [ "Other" ];
    })
  ];

  xdg.mimeApps.defaultApplications.${mimeType} = [ "${name}.desktop" ];
}

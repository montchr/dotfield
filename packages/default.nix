{self, ...}: let
  inherit (self.inputs) apparat flake-utils;
  inherit (apparat.lib.typography) fontWeights;
  inherit (flake-utils.lib) filterPackages;
in {
  perSystem = {
    inputs',
    pkgs,
    system,
    ...
  }: let
    inherit (inputs') emacs-overlay;
    inherit (pkgs) callPackage;
    makeIosevkaXtalPackage = import ./data/fonts/iosevka-xtal/package.nix {inherit fontWeights;};
  in {
    packages = filterPackages system {
      ast-grep = callPackage ./development/tools/misc/ast-grep {};
      base16-schemes = callPackage ./data/themes/base16-schemes {};
      cod = callPackage ./shells/cod {};
      emacs-plus-29 = callPackage ./applications/editors/emacs/emacs-plus-29.nix {
        inherit (emacs-overlay.packages) emacs-unstable;
      };
      emacs-plus-edge = callPackage ./applications/editors/emacs/emacs-plus-edge.nix {
        inherit (emacs-overlay.packages) emacs-git;
      };
      epson-201212w = callPackage ./misc/drivers/epson_201212w {};
      ddi = callPackage ./tools/system/dd/ddi.nix {};
      firefox-ui-fix = callPackage ./data/themes/firefox-ui-fix {};
      fd = callPackage ./tools/misc/fd {};
      igr = callPackage ./tools/text/igr {};
      kitty-get-window-by-platform-id = callPackage ./applications/terminal-emulators/kitty/get-window-by-platform-id.nix {};
      synadm = callPackage ./development/tools/misc/synadm {};

      ##: fonts
      berkeley-mono = callPackage ./data/fonts/berkeley-mono/default.nix {};
      iosevka-xtal = callPackage (makeIosevkaXtalPackage {}) {};
      iosevka-xtal-term = callPackage (makeIosevkaXtalPackage {
        familySuffix = "Term";
        spacing = "term";
        slopes = ["italic"];
        weights = ["light" "regular" "semibold" "bold"];
      }) {};
      sf-pro = callPackage ./data/fonts/sf-pro {};
    };
  };
}

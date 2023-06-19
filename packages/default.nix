{self, ...}: let
  inherit (self.inputs.flake-utils.lib) filterPackages;
in {
  perSystem = {
    inputs',
    pkgs,
    system,
    ...
  } @ ctx: let
    inherit (inputs') emacs-overlay;
    inherit (pkgs) callPackage;
  in {
    packages = filterPackages system {
      ast-grep = callPackage ./development/tools/misc/ast-grep {};
      base16-schemes = callPackage ./data/themes/base16-schemes {};
      berkeley-mono = callPackage ./data/fonts/berkeley-mono/default.nix {};
      black = callPackage ./development/python-modules/black {};
      cod = callPackage ./shells/cod {};
      ddi = callPackage ./tools/system/dd/ddi.nix {};
      emacs-plus-29 = callPackage ./applications/editors/emacs/emacs-plus-29.nix {
        inherit (emacs-overlay.packages) emacs-unstable;
      };
      emacs-plus-edge = callPackage ./applications/editors/emacs/emacs-plus-edge.nix {
        inherit (emacs-overlay.packages) emacs-git;
      };
      epson-201212w = callPackage ./misc/drivers/epson_201212w {};
      firefox-ui-fix = callPackage ./data/themes/firefox-ui-fix {};
      # FIXME: fails to build b/c python...
      # (specifically the `ruff` module, which is really written in Rust but re-distributed as a python module which tries to run `cargo install` upon installation, which fails b/c that breaks reproducible builds...)
      gpt-engineer = callPackage ./development/tools/misc/gpt-engineer {
        inherit (ctx.config.packages) black ruff;
      };
      igr = callPackage ./tools/text/igr {};
      kitty-get-window-by-platform-id = callPackage ./applications/terminal-emulators/kitty/get-window-by-platform-id.nix {};
      ruff = callPackage ./development/python-modules/ruff {};
      sf-pro = callPackage ./data/fonts/sf-pro {};
    };
  };
}

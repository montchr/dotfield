{ self, ... }:
let
  inherit (self.inputs) apparat flake-utils;
  inherit (apparat.lib.typography) fontWeights;
  inherit (flake-utils.lib) filterPackages;
in
{
  perSystem =
    {
      inputs',
      pkgs,
      system,
      ...
    }:
    let
      inherit (inputs') emacs-overlay;
      inherit (pkgs) callPackage callPackages;
    in
    {
      packages = filterPackages system (
        {
          ast-grep = callPackage ./development/tools/misc/ast-grep { };
          base16-schemes = callPackage ./data/themes/base16-schemes { };
          cod = callPackage ./shells/cod { };
          emacs-plus-29 = callPackage ./applications/editors/emacs/emacs-plus-29.nix {
            inherit (emacs-overlay.packages) emacs-unstable;
          };
          emacs-plus-edge = callPackage ./applications/editors/emacs/emacs-plus-edge.nix {
            inherit (emacs-overlay.packages) emacs-git;
          };
          epson-201212w = callPackage ./misc/drivers/epson_201212w { };
          ddi = callPackage ./tools/system/dd/ddi.nix { };
          firefox-ui-fix = callPackage ./data/themes/firefox-ui-fix { };
          fzf-tab-completion = callPackage ./shells/fzf-tab-completion/package.nix { };

          ##: gh cli extensions
          gh-i = callPackage ./by-name/gh/gh-i/package.nix { };
          gh-repo-explore = callPackage ./by-name/gh/gh-repo-explore/package.nix { };
          gh-s = callPackage ./by-name/gh/gh-s/package.nix { };

          git-repo-manager = callPackage ./git-repo-manager/package.nix { };
          igr = callPackage ./tools/text/igr { };
          kitty-get-window-by-platform-id =
            callPackage ./applications/terminal-emulators/kitty/get-window-by-platform-id.nix
              { };
          kitty-grab = callPackage ./tools/misc/kitty-grab/package.nix { };
          tomlfmt = callPackage ./by-name/to/tomlfmt/package.nix { };
          synadm = callPackage ./development/tools/misc/synadm { };

          ##: fonts
          berkeley-mono = callPackage ./data/fonts/berkeley-mono/default.nix { };
          sf-pro = callPackage ./data/fonts/sf-pro { };
        }
        // (callPackages ./data/fonts/iosevka-xtal/packages.nix { inherit fontWeights; })
      );
    };
}

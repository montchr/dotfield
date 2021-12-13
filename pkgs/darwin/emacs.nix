{ emacsPgtkGcc, fetchFromGitHub, lib, ... }:

let
  emacsPlus = fetchFromGitHub {
    owner = "d12frosted";
    repo = "homebrew-emacs-plus";
    # TODO: double-check this revision!
    rev = "aac87685c893e3de07f77c077bcaf3403f1ceb8a";
    # FIXME: commented out until revision is verified
    # sha256 = "1d3znwhcafxlwp8bqzwy07mgwr868z22pfnivw88213sp10v5zsk";
  };
in

# TODO: is the first 'override' necessary here? currently it appears unused.
((emacsPgtkGcc.override { }).overrideAttrs
  ({ patches ? [ ]
   , postPatch ? ""
   , ...
   }: {
    patches = patches ++ [
      "${emacsPlus}/patches/emacs-28/fix-window-role.patch"
      "${emacsPlus}/patches/emacs-28/no-frame-refocus-cocoa.patch"
      "${emacsPlus}/patches/emacs-28/system-appearance.patch"
    ];
    postPatch = ''
      ${postPatch}
      cp -f ${emacsPlus}/icons/nobu417-big-sur.icns nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns
    '';
  }))

# via:
# https://github.com/sei40kr/dotfiles/blob/94ebb6211545949e6967a2834426eee65b7546a0/packages/all/emacs.nix

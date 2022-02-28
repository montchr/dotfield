final: prev:

let
  emacsPlus = final.fetchFromGitHub {
    owner = "d12frosted";
    repo = "homebrew-emacs-plus";
    rev = "b7809dd815e7753e20851c81603c82a573d7d1cc";
    sha256 = "sha256-UoMieQKaWB9vSQ75866Kpjb0OKbO1OOj9IwKdAFQit4=";
  };
in

{
  # prefmanager = inputs.prefmanager.defaultPackage.${prev.stdenv.system};
  yabai = final.callPackage (import ./yabai.nix) { };

  emacs29Darwin = ((prev.emacsPackagesFor prev.emacsPgtkGcc).emacsWithPackages (epkgs: [
    epkgs.vterm
  ]));

  emacsGcc = (prev.emacsGcc.overrideAttrs (o: {
    # TODO: add no-titlebar patch! oops.
    patches = o.patches ++ [
      "${emacsPlus}/patches/emacs-28/fix-window-role.patch"
      "${emacsPlus}/patches/emacs-28/no-frame-refocus-cocoa.patch"
      "${emacsPlus}/patches/emacs-28/system-appearance.patch"
    ];
    # TODO: give this a try on next rebuild
    # postPatch = ''
    #   ${postPatch}
    #   cp -f ${emacsPlus}/icons/nobu417-big-sur.icns nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns
    # '';
  }));
}

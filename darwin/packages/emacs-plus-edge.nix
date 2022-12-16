{
  lib,
  emacsGit,
  fetchFromGitHub,
  icon ? null,
}: let
  emacsPlus = fetchFromGitHub {
    owner = "d12frosted";
    repo = "homebrew-emacs-plus";
    rev = "61d588ce80fb4282e107f5ab97914e32451c3da1";
    sha256 = "";
  };
  patchesDir = emacsPlus + "/patches/emacs-30";
in
  emacsGit.overrideAttrs (o: {
    pname = "emacsPlusEdge";

    patches = [
      "${patchesDir}/fix-window-role.patch"
      "${patchesDir}/system-appearance.patch"
      "${patchesDir}/poll.patch"
    ];

    # https://github.com/d12frosted/homebrew-emacs-plus#icons
    postPatch =
      o.postPatch
      + lib.optionalString (icon != null) ''
        cp -f ${emacsPlus}/icons/${icon}.icns nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns
      '';

    # https://github.com/siraben/nix-gccemacs-darwin/blob/f543cf1d30dc8afb895aaddfb73c92cb739874fe/emacs.nix#L27-L29
    postInstall =
      o.postInstall
      + ''
        ln -snf $out/lib/emacs/${o.version}/native-lisp $out/Applications/Emacs.app/Contents/native-lisp
      '';
  })

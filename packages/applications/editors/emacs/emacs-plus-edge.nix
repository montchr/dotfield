{
  lib,
  emacs-git,
  fetchFromGitHub,
  icon ? null,
}:
let
  emacs-plus = fetchFromGitHub {
    owner = "d12frosted";
    repo = "homebrew-emacs-plus";
    rev = "61d588ce80fb4282e107f5ab97914e32451c3da1";
    sha256 = "sha256-xJYEGu9wnndEhUTMVKiFPL1jwq+T6yEoZdYi5A1TTFQ=";
  };
  patchesDir = emacs-plus + "/patches/emacs-30";
in
emacs-git.overrideAttrs (o: {
  pname = "emacs-plus-edge";

  patches = [
    "${patchesDir}/fix-window-role.patch"
    "${patchesDir}/system-appearance.patch"
    "${patchesDir}/poll.patch"
  ];

  # https://github.com/d12frosted/homebrew-emacs-plus#icons
  postPatch =
    o.postPatch
    + lib.optionalString (icon != null) ''
      cp -f ${emacs-plus}/icons/${icon}.icns nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns
    '';

  # https://github.com/siraben/nix-gccemacs-darwin/blob/f543cf1d30dc8afb895aaddfb73c92cb739874fe/emacs.nix#L27-L29
  postInstall =
    o.postInstall
    + ''
      ln -snf $out/lib/emacs/${o.version}/native-lisp $out/Applications/Emacs.app/Contents/native-lisp
    '';

  meta.platforms = [
    "x86_64-darwin"
    "aarch64-darwin"
  ];
})

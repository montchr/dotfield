{
  lib,
  emacs-unstable,
  fetchFromGitHub,
  ##: options
  withTitleBar ? true,
  withRoundCorners ? false,
  withFrameRefocus ? true,
  icon ? null,
}:
let
  emacs-plus = fetchFromGitHub {
    owner = "d12frosted";
    repo = "homebrew-emacs-plus";
    rev = "b926ff102067d3864ff4cb8060962ec4d46510ef";
    sha256 = "sha256-WAGKPYOphID1HJHk/pyDxv/fvWUNqUjL6KL/7eyyC0A=";
  };
  patchesDir = emacs-plus + "/patches/emacs-29";
in
emacs-unstable.overrideAttrs (o: {
  pname = "emacs-plus-${o.version}";

  patches =
    lib.optional (!withTitleBar && withRoundCorners) "${patchesDir}/no-titlebar-and-round-corners.patch"
    ++ lib.optional (!withTitleBar && !withRoundCorners) "${patchesDir}/no-titlebar.patch"
    ++ lib.optional (!withFrameRefocus) "${patchesDir}/no-frame-refocus.patch"
    ++ [
      "${patchesDir}/fix-window-role.patch"
      "${patchesDir}/system-appearance.patch"
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

{
  lib,
  emacsUnstable,
  fetchFromGitHub,
  ##: options
  withTitleBar ? true,
  withRoundCorners ? false,
  withFrameRefocus ? true,
  icon ? null,
}: let
  emacsPlus = fetchFromGitHub {
    owner = "d12frosted";
    repo = "homebrew-emacs-plus";
    rev = "85c84fd60418aaed3be3972728380ab890311dd3";
    sha256 = "sha256-IdnQm1i+jgxSa/8P6BX020waXrXEDbOKLI+bjq3RaxM=";
  };
  patchesDir = emacsPlus + "/patches/emacs-28";
in
  emacsUnstable.overrideAttrs (o: {
    pname = "emacsPlusNativeComp";

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
        cp -f ${emacsPlus}/icons/${icon}.icns nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns
      '';

    # https://github.com/siraben/nix-gccemacs-darwin/blob/f543cf1d30dc8afb895aaddfb73c92cb739874fe/emacs.nix#L27-L29
    postInstall =
      o.postInstall
      + ''
        ln -snf $out/lib/emacs/${o.version}/native-lisp $out/Applications/Emacs.app/Contents/native-lisp
      '';
  })

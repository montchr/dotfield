{
  emacsNativeComp,
  fetchFromGitHub,
  darwin,
}:
let
  emacsPlus = fetchFromGitHub {
    owner = "d12frosted";
    repo = "homebrew-emacs-plus";
    rev = "b7809dd815e7753e20851c81603c82a573d7d1cc";
    sha256 = "sha256-UoMieQKaWB9vSQ75866Kpjb0OKbO1OOj9IwKdAFQit4=";
  };
in
emacsNativeComp.overrideAttrs (o: {
  pname = "emacsPlusNativeComp";

  # https://github.com/cmacrae/emacs/blob/03b4223e56e10a6d88faa151c5804d30b8680cca/flake.nix#L75
  buildInputs = o.buildInputs ++ [darwin.apple_sdk.frameworks.WebKit];

  # https://github.com/siraben/nix-gccemacs-darwin/blob/f543cf1d30dc8afb895aaddfb73c92cb739874fe/emacs.nix#L16-L17
  configureFlags =
    o.configureFlags
    ++ ["--with-cairo" "--with-harfbuzz"];

  patches = [
    "${emacsPlus}/patches/emacs-28/no-titlebar.patch"
    "${emacsPlus}/patches/emacs-28/fix-window-role.patch"
  ];

  # https://github.com/d12frosted/homebrew-emacs-plus#icons
  postPatch = ''
    ${o.postPatch}
    cp -f ${emacsPlus}/icons/nobu417-big-sur.icns nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns
  '';

  # https://github.com/siraben/nix-gccemacs-darwin/blob/f543cf1d30dc8afb895aaddfb73c92cb739874fe/emacs.nix#L27-L29
  postInstall =
    o.postInstall
    + ''
      ln -snf $out/lib/emacs/${o.version}/native-lisp $out/Applications/Emacs.app/Contents/native-lisp
    '';
})

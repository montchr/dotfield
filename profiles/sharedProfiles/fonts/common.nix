{
  pkgs,
  flake,
  ...
}: let
  inherit (flake.perSystem) packages;
  inherit (pkgs.stdenv.hostPlatform) isLinux isMacOS;
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  imports = [
    ./iosevka-xtal.nix
    ./iosevka-variants.nix
  ];

  fonts.fontDir.enable = true;
  fonts.fonts =
    (with pkgs; [
      dejavu_fonts

      # <https://software.sil.org/charis/>
      # <https://practicaltypography.com/charter.html>
      charis-sil

      emacs-all-the-icons-fonts
      # <https://bboxtype.com/typefaces/FiraSans/>
      fira
      ia-writer-duospace
      ibm-plex
      inter
      jetbrains-mono
      (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
    ])
    ++ (l.optionals isLinux (with pkgs; [
      bakoma_ttf
      corefonts # broken on aarch64-darwin
      gentium
      liberation_ttf
      terminus_font # broken on aarch64-darwin
    ]))
    ++ (l.optional isMacOS packages.sf-pro);
}

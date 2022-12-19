{
  inputs,
  pkgs,
  packages,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux isMacOS;
  l = inputs.nixpkgs.lib // builtins;
in {
  imports = [
    ./iosevka-variants.nix
    ./iosevka-xtal.nix
  ];
  fonts.fontDir.enable = true;
  fonts.fonts =
    (with pkgs; [
      emacs-all-the-icons-fonts
      ibm-plex
      inter
      (nerdfonts.override {
        fonts = [
          "Iosevka"
          "NerdFontsSymbolsOnly"
        ];
      })
    ])
    ++ (l.optionals isLinux (with pkgs; [
      bakoma_ttf
      corefonts # broken on aarch64-darwin
      dejavu_fonts
      gentium
      liberation_ttf
      terminus_font # broken on aarch64-darwin
    ]))
    ++ (l.optional isMacOS packages.sf-pro);
}

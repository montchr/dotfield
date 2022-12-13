{
  inputs,
  pkgs,
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
      dejavu_fonts
      emacs-all-the-icons-fonts
      ibm-plex
      inter
      liberation_ttf
      terminus_font # we are the robots
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
      gentium
    ]))
    ++ (l.optional isMacOS pkgs.sf-pro);
}

{flake, ...}: let
  # inherit
  #   (flake.perSystem.inputs'.iosevka-xtal.packages)
  #   iosevka-xtal
  #   iosevka-xtal-term
  #   ;
  inherit
    (flake.perSystem.packages)
    iosevka-xtal
    iosevka-xtal-term
    ;
in {
  fonts.fonts = [iosevka-xtal iosevka-xtal-term];
  nix.settings = {
    substituters = ["https://iosevka-xtal.cachix.org"];
    trusted-substituters = ["https://iosevka-xtal.cachix.org"];
    trusted-public-keys = ["iosevka-xtal.cachix.org-1:5d7Is01fs3imwU9w5dom2PcSskJNwtJGbfjRxunuOcw="];
  };
}

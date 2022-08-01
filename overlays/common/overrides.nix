channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-unstable)
    fish
    kitty
    python310Packages
    ;

  inherit
    (channels.nixpkgs-trunk)
    iosevka
    iosevka-bin
    iosevka-comfy
    iosevka-xtal
    iosevka-xtal-term
    nerdfonts
    ;

  ripgrep = prev.ripgrep.override {withPCRE2 = true;};

  # FIXME: https://github.com/NixOS/nixpkgs/issues/175875
  httpie = final.python310Packages.httpie;
}

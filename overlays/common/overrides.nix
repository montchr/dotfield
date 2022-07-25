channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-unstable)
    python310Packages
    kitty
    ;

  inherit
    (channels.nixpkgs-trunk)
    iosevka
    iosevka-bin
    iosevka-comfy
    iosevka-seadome
    nerdfonts
    ;

  ripgrep = prev.ripgrep.override {withPCRE2 = true;};

  # FIXME: https://github.com/NixOS/nixpkgs/issues/175875
  httpie = final.python310Packages.httpie;
}

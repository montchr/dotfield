channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-unstable)
    iosevka
    iosevka-bin
    iosevka-comfy
    nerdfonts
    python310Packages
    kitty
    ;

  ripgrep = prev.ripgrep.override {withPCRE2 = true;};

  # FIXME: https://github.com/NixOS/nixpkgs/issues/175875
  httpie = final.python310Packages.httpie;
}

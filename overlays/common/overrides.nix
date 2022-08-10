channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-unstable)
    fish
    iosevka
    iosevka-bin
    iosevka-comfy
    iosevka-xtal
    iosevka-xtal-term
    kitty
    nerdfonts
    python310Packages
    ;

  inherit
    (channels.nixos-unstable.nodePackages)
    pyright
    ;

  ripgrep = prev.ripgrep.override {withPCRE2 = true;};

  # FIXME: https://github.com/NixOS/nixpkgs/issues/175875
  httpie = final.python310Packages.httpie;
}

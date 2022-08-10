channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-unstable)
    fish
    kitty
    nerdfonts
    python310Packages
    ;

  # FIXME: https://github.com/NixOS/nixpkgs/issues/185633
  inherit
    (channels.nixos-unstable-iosevka-185633)
    iosevka
    iosevka-bin
    iosevka-comfy
    ;

  inherit
    (channels.nixos-unstable.nodePackages)
    pyright
    ;

  ripgrep = prev.ripgrep.override {withPCRE2 = true;};

  # FIXME: https://github.com/NixOS/nixpkgs/issues/175875
  httpie = final.python310Packages.httpie;
}

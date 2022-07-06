channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-unstable)
    awscli2
    iosevka
    iosevka-bin
    iosevka-comfy
    nerdfonts
    kitty
    xplr
  ;

  ripgrep = prev.ripgrep.override {withPCRE2 = true;};
}

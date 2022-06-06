channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-unstable)
    awscli2
    kitty
    xplr
    ;

  ripgrep = prev.ripgrep.override {withPCRE2 = true;};
}

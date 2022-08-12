channels: final: prev: {
  __dontExport = true;

  # FIXME: https://github.com/NixOS/nixpkgs/issues/185633
  inherit
    (channels.nixos-unstable-iosevka-185633)
    iosevka
    iosevka-bin
    iosevka-comfy
    ;
}

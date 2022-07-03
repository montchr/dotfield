channels: final: prev: {
  __dontExport = true;

  inherit
    (channels.nixos-stable)
    pass-secret-service
    ;
}

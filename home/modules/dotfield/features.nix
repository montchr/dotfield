{ lib, ... }:
{
  options.dotfield.features = lib.mkOption {
    type = lib.types.attrsOf lib.types.unspecified;
    default = { };
  };

  config.dotfield.features = {
    hasWayland = lib.mkDefault true;
  };
}

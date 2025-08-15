{ config, ... }:
{
  dotfield.features.home.nixos = {
    imports = with config.dotfield.features.nixos; [ ];
  };
}

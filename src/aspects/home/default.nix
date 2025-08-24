{ config, ... }:
{
  dotfield.aspects.home.nixos = {
    imports = with config.dotfield.aspects.nixos; [ ];
  };
}

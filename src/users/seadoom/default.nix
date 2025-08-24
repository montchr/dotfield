{ config, ... }:
let
  inherit (config.dotfield) aspects meta;
in
{
  dotfield.users.seadoom = {
    baseline = {
      aspects = [

      ];
    };
  };

  dotfield.meta.users.seadoom = meta.users.cdom;
}

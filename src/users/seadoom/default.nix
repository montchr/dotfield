{ config, ... }:
let
  inherit (config.dotfield) aspects meta;
in
{
  dotfield.users.seadoom = { };

  dotfield.meta.users.seadoom = meta.users.cdom;
}

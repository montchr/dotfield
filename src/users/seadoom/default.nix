{ config, ... }:
let
  inherit (config) meta;
in
{
  meta.users.seadoom = meta.users.cdom;
}

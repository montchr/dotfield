{ config, ... }:
let
  inherit (config) meta;
in
{
  users.seadoom = { };
  meta.users.seadoom = meta.users.cdom;
}

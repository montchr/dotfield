{ config, ops, ... }:
let
  inherit (config.networking) hostName;
in
{
  services.tailscale.enable = true;

  networking.domain = ops.hosts.${hostName}.domain;
}

{ config, ops, ... }:
let
  inherit (config.networking) hostName;
in
{
  networking.domain = ops.hosts.${hostName}.domain;
}

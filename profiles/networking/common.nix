{
  config,
  lib,
  pkgs,
  peers,
  ...
}: let
  inherit (config.networking) hostName;
  inherit (netMeta) domain;

  hostMeta = peers.hosts.${hostName};
  hostNet = hostMeta.network;
  netMeta = peers.networks.${hostNet};
in
  lib.mkMerge [
    {
      networking = {
        # Use Cloudflare DNS
        # https://developers.cloudflare.com/1.1.1.1/
        nameservers = [
          "1.1.1.1"
          "1.0.0.1"
          "2606:4700:4700::1111"
          "2606:4700:4700::1001"
        ];
      };
    }
    (lib.mkIf (config.networking ? domain) {
      networking = {inherit domain;};
    })
  ]

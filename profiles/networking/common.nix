{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.networking) hostName;
  hostNet =
    (lib.our.peers.getHost hostName).network
    or (config.nixos-vm.peerConfig).network;
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
      networking.domain = (lib.our.peers.getNet hostNet).domain or null;
    })
  ]

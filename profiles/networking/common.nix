{
  config,
  lib,
  pkgs,
  self,
  ...
}: let
  inherit (config.networking) hostName;
  inherit (self.lib.peers) getHost getNet;
  hostNet = (getHost hostName).network or (config.nixos-vm.peerConfig).network or null;
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
      networking.domain = (getNet hostNet).domain or null;
    })
  ]

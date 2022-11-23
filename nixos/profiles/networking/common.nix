{
  config,
  lib,
  self,
  ...
}: let
  inherit (self.lib.peers) getHost getNet;
  inherit (lib) mkIf;
  inherit (config.networking) hostName;

  # FIXME: this host/peer stuff is a total mess! terrible ux, always breaks...
  hostNet = (getHost hostName).network or (config.nixos-vm.peerConfig).network or null;
  hostDomain = (getNet hostNet).domain or null;
in {
  networking.domain = mkIf (hostDomain != null) hostDomain;
  networking.nameservers = [
    "1.1.1.1"
    "1.0.0.1"
    "2606:4700:4700::1111"
    "2606:4700:4700::1001"
  ];
}

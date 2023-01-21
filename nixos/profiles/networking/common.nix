{
  inputs,
  config,
  peers,
  ...
}: let
  inherit (config.networking) hostName;
  l = inputs.nixpkgs.lib // builtins;

  # FIXME: this host/peer stuff is a total mess! terrible ux, always breaks...
  hostNet =
    peers.hosts.${hostName}.network
    or (config.nixos-vm.peerConfig).network or null;
  hostDomain = peers.networks.${hostNet}.domain or null;
in {
  networking.domain = l.mkIf (hostDomain != null) hostDomain;
  networking.nameservers = [
    "1.1.1.1"
    "1.0.0.1"
    "2606:4700:4700::1111"
    "2606:4700:4700::1001"
  ];
}

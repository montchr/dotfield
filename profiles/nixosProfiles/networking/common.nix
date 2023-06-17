{
  flake,
  config,
  ops,
  ...
}: let
  inherit (ops.metadata) dns hosts networks;
  inherit (config.networking) hostName;
  l = flake.inputs.nixpkgs.lib // builtins;

  # FIXME: this host/peer stuff is a total mess! terrible ux, always breaks...
  hostNet = hosts.${hostName}.network or null;
  hostDomain = networks.${hostNet}.domain or null;
in {
  networking.domain = l.mkIf (hostDomain != null) hostDomain;
  networking.nameservers = dns.ns.quad9;
}

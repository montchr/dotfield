{
  flake,
  config,
  ops,
  ...
}: let
  inherit (ops.metadata) hosts networks;
  inherit (config.networking) hostName;
  l = flake.inputs.nixpkgs.lib // builtins;

  # FIXME: this host/peer stuff is a total mess! terrible ux, always breaks...
  hostNet = hosts.${hostName}.network or null;
  hostDomain = networks.${hostNet}.domain or null;
in {
  networking.domain = l.mkIf (hostDomain != null) hostDomain;
  # TODO: use quad9 - <https://www.quad9.net/>
  networking.nameservers = [
    "1.1.1.1"
    "1.0.0.1"
    "2606:4700:4700::1111"
    "2606:4700:4700::1001"
  ];
}

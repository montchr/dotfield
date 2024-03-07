{
  config,
  ops,
  flake,
  ...
}:
let
  inherit (config.networking) hostName;
  inherit (ops.hosts.${hostName}) ipv6;
  l = flake.inputs.nixpkgs.lib // builtins;
in
{
  # TODO: the value here can be generalised to any host, maybe make reusable
  systemd.network.networks."10-uplink".networkConfig.Address = "${ipv6.address}::/${l.toString ipv6.prefixLength}";
}

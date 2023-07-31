{
  config,
  ops,
  ...
}: let
  inherit (config.networking) hostName;
in {
  networking.domain = ops.metadata.hosts.${hostName}.domain;
}

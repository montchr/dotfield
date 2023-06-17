{
  config,
  ops,
  ...
}: let
  inherit (ops.metadata) hosts networks;
  inherit (config.networking) hostName;

  # FIXME: find a better way
  hostNet = hosts.${hostName}.network or null;
in {
  security.acme = {
    defaults.email = (networks.${hostNet} or networks.seadome).contact;
    acceptTerms = true;
  };
}

{
  config,
  ops,
  ...
}: let
  inherit (ops) hosts networks;
  inherit (config.networking) hostName;

  # FIXME: find a better way. a module? still, it's nice to have this as jsonable data.
  hostNet = hosts.${hostName}.network or "";
in {
  security.acme = {
    defaults.email = (networks.${hostNet} or networks.seadome).contact;
    acceptTerms = true;
  };
}

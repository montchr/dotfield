{
  lib,
  peers,
}:
lib.makeExtensible (self: {
  peers = rec {
    getHost = hostName: peers.hosts.${hostName} or false;
    getNet = network: peers.networks.${network} or false;
  };

  mkOpt = type: default: lib.mkOption {inherit type default;};

  mkOpt' = type: default: description:
    lib.mkOption {inherit type default description;};

  mkBoolOpt = default:
    lib.mkOption {
      inherit default;
      type = lib.types.bool;
      example = true;
    };
})

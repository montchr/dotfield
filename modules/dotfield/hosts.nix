{
  inputs,
  config,
  ...
}: let
  inherit (inputs.apparat.types) host;
  l = inputs.nixpkgs.lib // builtins;
  t = l.types;
  cfg = config.dotfield;
in {
  options.dotfield.hosts = l.mkOption {
    default = {};
    type = t.attrsOf host;
  };
  config = l.mkIf cfg.enable {};
}

{
  inputs,
  self,
  config,
  ...
}: let
  inherit (self.lib.types) host;
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
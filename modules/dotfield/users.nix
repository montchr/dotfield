{
  inputs,
  config,
  ...
}: let
  inherit (inputs.flib.lib.types) user;
  l = inputs.nixpkgs.lib // builtins;
  cfg = config.dotfield;
in {
  options.dotfield.users = l.mkOption {
    type = l.types.attrsOf user;
  };
  config = l.mkIf cfg.enable {
    # dotfield.users =
  };
}

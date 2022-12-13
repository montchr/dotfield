{inputs, ...}: let
  l = inputs.nixpkgs.lib // builtins;
in {
  mkOpt = type: default: l.mkOption {inherit type default;};

  mkOpt' = type: default: description:
    l.mkOption {inherit type default description;};

  mkBoolOpt = default:
    l.mkOption {
      inherit default;
      type = l.types.bool;
      example = true;
    };
}

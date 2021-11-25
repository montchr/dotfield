{ lib }:
lib.makeExtensible (self: {

  mkOpt = type: default: lib.mkOption { inherit type default; };

  mkOpt' = type: default: description:
    lib.mkOption { inherit type default description; };

  mkBoolOpt = default:
    lib.mkOption {
      inherit default;
      type = lib.types.bool;
      example = true;
    };

})

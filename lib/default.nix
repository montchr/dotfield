{lib}:
lib.makeExtensible (self: {
  isDarwin = system: (builtins.elem system lib.platforms.darwin);
  homePrefix = system:
    if self.isDarwin system
    then "/Users"
    else "/home";

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
##: sources
#
# https://github.com/kclejeune/system/blob/71c65173e7eba8765a3962df5b52c2f2c25a8fac/flake.nix#L59-L60


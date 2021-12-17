{ lib }:
lib.makeExtensible (self: {

  generators = import ./generators.nix { inherit self; };

  isGenericLinux = system: (builtins.elem system lib.platforms.linux);

  isDarwin = system: (builtins.elem system lib.platforms.darwin);

  homePrefix = system: if self.isDarwin system then "/Users" else "/home";

  /*
    Path to a user's home directory based on system home prefix.

    Example:

    # linux
    mkHomePath jane
    => "/home/jane"

    # darwin
    mkHomePath jane
    => "/Users/jane"
  */
  mkHomePath = name: "${self.homePrefix}/${name}";

  mkOpt = type: default: lib.mkOption { inherit type default; };

  mkOpt' = type: default: description:
    lib.mkOption { inherit type default description; };

  mkStrOpt = lib.mkOption { type = lib.types.str; };

  mkBoolOpt = default:
    lib.mkOption {
      inherit default;
      type = lib.types.bool;
      example = true;
    };

})

# via:
# https://github.com/kclejeune/system/blob/71c65173e7eba8765a3962df5b52c2f2c25a8fac/flake.nix#L59-L60

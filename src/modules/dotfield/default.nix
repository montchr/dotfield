{ self, lib, ... }:
let
  inherit (lib) mkOption types;
in

{
  imports = [
    ./modules.nix
    ./nixos.nix
  ];

  options.dotfield = {
    options = mkOption {
      type = types.lazyAttrsOf types.raw;
      default = { };
    };
  };
}

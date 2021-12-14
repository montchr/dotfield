{ ... }:
let
  inherit (default.inputs.nixpkgs-darwin-stable) lib;

  host = configs.${hostname};
  configs = default.darwinConfigurations;
  default = (import ../.).defaultNix;
  # FIXME: /etc/hostname doesn't exist on Darwin!
  hostname = lib.fileContents /etc/hostname;
in
host

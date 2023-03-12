{
  inputs,
  cell,
}: let
  inherit (inputs.cells) lib;
in {
  garnix = lib.cfg.garnix {
    data = import ./cfg/garnix.nix;
  };
}

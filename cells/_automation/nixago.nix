{
  inputs,
  cell,
}: let
  inherit (inputs.cells) lib;
in {
  garnix = lib.nixago.garnix {
    data = import ./nixago/garnix.nix;
  };
}

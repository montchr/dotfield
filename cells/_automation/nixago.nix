{
  inputs,
  cell,
}: let
  inherit (inputs.cells) lib;
in {
  garnix = lib.nixago.garnix {
    configData = import ./nixago/garnix.nix;
  };
}

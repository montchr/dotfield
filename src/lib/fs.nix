{ self, ... }:
let
  inherit (self.lib.fs) tree;
in
{
  flake.lib.fs = {
    importTree = tree;
  };
}

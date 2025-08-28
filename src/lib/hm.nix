{
  self,
  inputs,
  withSystem,
  ...
}:
let
  inherit (self.lib.modules) flakeSpecialArgs flakeSpecialArgs';

  specialArgs = {
    flake = flakeSpecialArgs;
  };
  specialArgs' =
    system:
    specialArgs
    // {
      flake = flakeSpecialArgs // (flakeSpecialArgs' system);
    };
in
{
  flake.lib.hm = {
    inherit
      specialArgs'
      ;
  };
}

{ flake, withSystem, ... }:
let
  flakeSpecialArgs = flake;
  flakeSpecialArgs' =
    system:
    withSystem system (
      { inputs', ... }@ctx:
      let
        perSystem = {
          inherit (ctx.config) legacyPackages packages;
          inherit inputs';
        };
      in
      flakeSpecialArgs // { inherit perSystem; }
    );
in
{
  inherit flakeSpecialArgs flakeSpecialArgs';
}

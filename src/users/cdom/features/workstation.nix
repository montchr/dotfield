{ config, ... }:
let
  aspects = config.dotfield.users.cdom.aspects;
in
{
  dotfield.users.cdom.aspects.workstation.home =
    { pkgs, ... }:
    {
      imports = [
        aspects.password-store.home
      ];
    };
}

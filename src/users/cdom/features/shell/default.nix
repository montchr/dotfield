{
  users.cdom.aspects.core.home =
    { pkgs, ... }:
    let
      shellAliases = import ./__aliases.nix { inherit pkgs; };
    in
    {
      home = {
        inherit shellAliases;
      };
    };
}

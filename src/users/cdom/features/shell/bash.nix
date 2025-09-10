{
  users.cdom.aspects.core.home =
    { pkgs, ... }:
    let
      shellAbbrs = import ./__abbrs.nix { inherit pkgs; };
    in
    {
      programs.bash.shellAliases = shellAbbrs;
    };
}

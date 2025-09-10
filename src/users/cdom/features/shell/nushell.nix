{
  users.cdom.aspects.core.home =
    { pkgs, ... }:
    let
      shellAliases = import ./__aliases.nix { inherit pkgs; };
      shellAbbrs = import ./__abbrs.nix { inherit pkgs; };
    in
    {
      # FIXME: `home.shellAliases` and `home.sessionVariables` are not propagated
      #        into the nushell session.  we get by without `home.sessionVariables`
      #        thanks to the trampoline.
      programs.nushell.shellAliases = shellAbbrs // shellAliases;
    };
}

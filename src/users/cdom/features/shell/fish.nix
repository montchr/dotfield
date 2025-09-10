{ self, ... }:
{
  users.cdom.aspects.core.home =
    { pkgs, ... }:
    let
      shellAbbrs = import ./__abbrs.nix { inherit pkgs; };
    in
    {
      programs.fish = {
        inherit shellAbbrs;
        plugins =
          [
            pkgs.fishPlugins.autopair
            pkgs.fishPlugins.done
          ]
          |> builtins.map self.lib.fish.toPluginAttrs;
      };
    };
}

{ lib, self, ... }:
let
  lib' = self.lib;
in
{
  dotfield.home =
    { pkgs, ... }:
    {
      programs.fish.plugins =
        (with pkgs.fishPlugins; [
          autopair
          done
        ])
        |> builtins.map lib'.fish.toPluginAttrs;
    };
}

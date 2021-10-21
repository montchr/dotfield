{ config, lib, pkgs, ... }:
let inherit (config) my; in
{
  my.hm.programs.fish.plugins = [
    {
      inherit (pkgs.sources.fish-nix-env) src;
      name = "nix-env";
    }
    {
      inherit (pkgs.sources.fish-z) src;
      name = "z";
    }
    # {
    #   inherit (pkgs.sources.fish-tide) src;
    #   name = "tide";
    # }
  ];
}

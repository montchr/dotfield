{ pkgs, lib, config, options, inputs, ... }:
with lib;
let
  inherit (inputs) base16-kitty;
  inherit (config) my;

  cfg = my.modules.kitty;
  themesCfg = my.modules.themes;
  configDir = "${config.dotfield.configDir}/kitty";
  socket = "unix:/tmp/kitty-socket";
in
{
  options = with lib; {
    my.modules.kitty = {
      enable = mkEnableOption ''
        Whether to enable kitty module
      '';
    } // (import ./options.nix { inherit config lib pkgs; });
  };
}

{ config, lib, pkgs, inputs, ... }:

# TODO: espanso doesn't install correctly on darwin! "unsupported system"
with lib;
let
  cfg = config.my.modules.espanso;
  plugins = [ "greek-letters-alt" ];
in {
  options = with lib; {
    my.modules.espanso = { enable = mkEnableOption false; };
  };

  config = mkIf cfg.enable {
    my = { user.packages = with pkgs; [ espanso ]; };

    system.activationScripts.postUserActivation.text = ''
      espanso register

      # TODO: results in error
      ${builtins.map (p: "espanso package install ${p}") plugins}
    '';
  };
}

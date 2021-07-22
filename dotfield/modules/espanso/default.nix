{ config, lib, pkgs, options, ... }:

with lib;
let
  cfg = config.my.modules.espanso;
  plugins = [ "greek-letters-alt" ];
in {
  options = with lib; {
    my.modules.espanso = { enable = mkEnableOption false; };
  };

  config = mkIf cfg.enable (mkMerge [
    (if (builtins.hasAttr "homebrew" options) then
      let cmd = "${config.homebrew.brewPrefix}/espanso";
      in {
        homebrew = {
          taps = [ "federico-terzi/espanso" ];
          brews = [ "espanso" ];
        };

        # Match the default agent configuration from espanso.
        launchd.user.agents.espanso = {
          serviceConfig = {
            ProgramArguments = [ cmd "daemon" ];
            RunAtLoad = true;
            StandardOutPath = "${config.my.xdgPaths.cache}/espanso.out.log";
            StandardErrorPath = "${config.my.xdgPaths.cache}/espanso.err.log";
          };
        };

        system.activationScripts.postUserActivation.text = ''
          # Restart the daemon.
          ${cmd} restart

          # Install espanso plugins.
          ${toString (map (p: "${cmd} package install ${p}") plugins)}
        '';
      }
    else {
      my.user.packages = with pkgs; [ espanso ];
    })
  ]);
}

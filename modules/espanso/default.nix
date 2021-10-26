{ config, lib, pkgs, options, ... }:

with lib;
let
  inherit (config) dotfield my;

  cfg = my.modules.espanso;
  configDirPath = "${dotfield.configDir}/espanso";
  snippetsDir = "${my.xdg.config}/espanso/user";
  plugins = [ "greek-letters-alt" ];
  secrets = (map (s: "${configDirPath}/user/${s}") [ ]);
  cmd = "espanso";
in
{
  options = with lib; {
    my.modules.espanso = { enable = mkEnableOption false; };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      system.activationScripts.postUserActivation.text = ''
        # Link decrypted secret configs to the Espanso snippets directory,
        # bypassing Nix so secrets don't pass through the store.
        # FIXME: bring these back
        # mkdir -p "${snippetsDir}"
        # ln -sfv ${toString secrets} ${snippetsDir}

        # Restart the daemon.
        # ${cmd} restart

        # Install espanso plugins.
        # ${toString (map (p: "${cmd} package install ${p}") plugins)}
      '';
    }
  ]);
}

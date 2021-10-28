{ config, lib, pkgs, ... }:
let
  shellCfg = config.my.modules.shell;
  configDir = "${config.dotfield.configDir}/zsh";

  aliasLines = lib.mapAttrsToList
    (n: v: ''alias ${n}="${v}"'')
    (shellCfg.abbrs // shellCfg.aliases);
in
{
  imports = [
    ../shell
  ];

  my.user.packages = with pkgs; [
    zsh
    zoxide
  ];

  my.env = rec {
    # zsh paths
    ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
    ZSH_CACHE = "$XDG_CACHE_HOME/zsh";
    ZSH_DATA = "$XDG_DATA_HOME/zsh";

    # zgenom paths
    ZGEN_DIR = "$XDG_DATA_HOME/zsh/sources";
    ZGEN_SRC_DIR = "$XDG_DATA_HOME/zsh/zgenom";
  };

  # Load starship on our own terms.
  my.hm.programs.starship.enableZshIntegration = false;

  my.hm.xdg.configFile = {
    "zsh" = {
      source = configDir;
      recursive = true;
      onChange = ''
        # Remove compiled files.
        fd -uu \
          --extension zwc \
          --exec \
            rm '{}'
      '';
    };

    "zsh/profile.zshenv".source = "${config.dotfield.libDir}/profile.sh";

    "zsh/extra.zshrc".text = ''
      # ${config.my.nix_managed}
      ${lib.concatStringsSep "\n" aliasLines}
      ${lib.concatMapStrings (path: "source '${path}'") shellCfg.rcFiles}
      ${shellCfg.rcInit}
    '';

    "zsh/extra.zshenv".text = ''
      # ${config.my.nix_managed}
      ${lib.concatMapStrings (path: "source '${path}'") shellCfg.envFiles}
      ${shellCfg.envInit}
    '';
  };
}

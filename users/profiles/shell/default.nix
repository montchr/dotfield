{ config, lib, pkgs, inputs, ... }:

let
  inherit (config) dotfield my;

  hmConfig = config.home-manager.users.${config.my.username};
  shellAliases =
    (import ./abbrs.nix)
    // (import ./aliases.nix);
in

{
  imports = [
    ./fzf.nix
  ];

  my.hm.programs.starship.enable = true;
  my.hm.programs.starship.enableZshIntegration = false;

  my.hm.home.packages = with pkgs; [
    zsh
    zoxide
  ];

  my.hm.xdg.configFile = {
    "starship".source = "${dotfield.configDir}/starship";
  };

  my.hm.programs.bash = {
    inherit shellAliases;

    enable = true;
    bashrcExtra = ''
      ${builtins.readFile ./env-init.sh}
    '';
    profileExtra = "";

    sessionVariables = {
      BASH_COMPLETION_USER_FILE = "${hmConfig.xdg.dataHome}/bash/completion";
    };
  };

  my.hm.programs.zsh = {
    inherit shellAliases;

    enable = true;
    dotDir = ".config/zsh";
    history.path = "${hmConfig.xdg.dataHome}/zsh/history";
    history.extended = true;
    history.ignoreDups = true;

    # These are handled by z4h.
    enableCompletion = false;
    enableSyntaxHighlighting = false;

    envExtraFirst = ''
      ${builtins.readFile ./env-z4h.zsh}
    '';
    envExtra = ''
      ${builtins.readFile ./env-init.sh}
    '';

    initExtraFirst = ''
      # Load our custom z4h config directly
      source $DOTFIELD_DIR/config/zsh/main.zsh
    '';

    sessionVariables = {
      ZSH_CACHE = "${hmConfig.xdg.cacheHome}/zsh";
      ZSH_DATA = "${hmConfig.xdg.dataHome}/zsh";
    };
  };

  my.hm.home.sessionVariables = {
    PATH = [ "$XDG_BIN_HOME" "$PATH" ];
    INPUTRC = "$XDG_CONFIG_HOME/readline/inputrc";
    COMPOSER_HOME = "$XDG_STATE_HOME/composer";
    LESSHISTFILE = "$XDG_STATE_HOME/lesshst";
    WGETRC = "$XDG_CONFIG_HOME/wgetrc";
    Z_DATA = "$XDG_DATA_HOME/z";

    # Docker
    DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
    MACHINE_STORAGE_PATH = "$XDG_DATA_HOME/docker-machine";

    # Go
    GOPATH = "$XDG_DATA_HOME/go";

    # Ruby
    BUNDLE_USER_CACHE = "$XDG_CACHE_HOME/bundle";
    BUNDLE_USER_CONFIG = "$XDG_CONFIG_HOME/bundle";
    BUNDLE_USER_PLUGIN = "$XDG_DATA_HOME/bundle";
    RBENV_ROOT = "$XDG_DATA_HOME/rbenv";

    # Rust
    CARGO_HOME = "$XDG_DATA_HOME/cargo";
    RUSTUP_HOME = "$XDG_DATA_HOME/rustup";

    # GNU screen
    SCREENRC = "$XDG_CONFIG_HOME/screen/screenrc";

    # Vagrant
    VAGRANT_ALIAS_FILE = "$XDG_DATA_HOME/vagrant/aliases";
    VAGRANT_HOME = "$XDG_DATA_HOME/vagrant";

    # wd
    # https://github.com/mfaerevaag/wd
    WD_CONFIG = "$XDG_CONFIG_HOME/wd/warprc";
  };
}

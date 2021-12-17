{ config, lib, pkgs, utils, ... }:
let
  inherit (config) dotfield my;

  extraVars = lib.concatStringsSep "\n"
    (lib.mapAttrsToList (n: v: ''export ${n}="${v}"'') my.env);
in
{
  imports = [
    ./fzf.nix
  ];

  shell = {
    abbrs = import ./abbrs.nix { inherit config lib pkgs; };
    aliases = import ./aliases.nix { inherit config lib pkgs; };
  };

  my.hm.programs.starship.enable = true;

  my.hm.xdg.configFile = lib.mkMerge [
    ({ "starship".source = "${dotfield.configDir}/starship"; })
  ];

  environment.extraInit = ''
    # Check whether a command exists.
    has() {
      type "$1" >/dev/null 2>&1
    }

    ${extraVars}
  '';

  my.env = {
    PATH = [ "$XDG_BIN_HOME" "$PATH" ];
    INPUTRC = "$XDG_CONFIG_HOME/readline/inputrc";
    COMPOSER_HOME = "$XDG_STATE_HOME/composer";
    LESSHISTFILE = "$XDG_STATE_HOME/lesshst";
    RIPGREP_CONFIG_PATH = "$XDG_CONFIG_HOME/ripgrep/config";
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

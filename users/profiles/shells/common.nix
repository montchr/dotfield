{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib.dotfield.whoami) githubUserName;

  shellAliases =
    (import ./abbrs.nix)
    // (import ./aliases.nix);

  fdBin = "${pkgs.fd}/bin/fd";
in {
  imports = [
    ./fzf.nix
    ./starship.nix
  ];

  programs.bash = {
    inherit shellAliases;

    enable = true;
    bashrcExtra = ''
      ${builtins.readFile ./env-init.sh}
    '';
    profileExtra = "";

    sessionVariables = {
      BASH_COMPLETION_USER_FILE = "${config.xdg.dataHome}/bash/completion";
    };
  };

  programs.exa.enable = true;
  programs.exa.enableAliases = true;
  programs.zoxide.enable = true;

  home.sessionVariables = {
    PATH = ["$XDG_BIN_HOME" "$PATH"];
    INPUTRC = "$XDG_CONFIG_HOME/readline/inputrc";

    # Default is "1". But when typeset in PragmataPro that leaves no space
    # between the icon and its filename.
    EXA_ICON_SPACING = "2";

    Z_OWNER = config.home.username;

    LESSHISTFILE = "$XDG_STATE_HOME/lesshst";
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

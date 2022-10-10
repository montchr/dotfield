# TODO: https://github.com/malob/nixpkgs/blob/f7a29deb3531d700e45efa30ee559cdcb48232f8/flake.nix#L77
moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.home) username;
  inherit (config.lib.dotfield.features) hasPragPro;
in {
  imports = [
    ./home-packages.nix
    ./nixpkgs-config.nix
  ];

  programs.bash.enable = true;
  programs.fish.enable = true;
  programs.zsh.enable = true;

  home.extraOutputsToInstall = ["/share/zsh"];

  home.sessionVariables = {
    LESSHISTFILE = "$XDG_STATE_HOME/lesshst";
    Z_DATA = "$XDG_DATA_HOME/z";
    Z_OWNER = username;

    # Docker
    DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
    MACHINE_STORAGE_PATH = "$XDG_DATA_HOME/docker-machine";

    # Go
    GOPATH = "$XDG_DATA_HOME/go";

    # Rust
    CARGO_HOME = "$XDG_DATA_HOME/cargo";
    RUSTUP_HOME = "$XDG_DATA_HOME/rustup";

    # GNU screen
    SCREENRC = "$XDG_CONFIG_HOME/screen/screenrc";

    # wd
    # https://github.com/mfaerevaag/wd
    WD_CONFIG = "$XDG_CONFIG_HOME/wd/warprc";
  };
}

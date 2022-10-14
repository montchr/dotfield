{config, ...}: {
  imports = [
    ./home-packages.nix
    ./nixpkgs-config.nix
  ];

  programs.home-manager.enable = true;
  manual.json.enable = true;
  news.display = "show";
  xdg.enable = true;

  programs.bash.enable = true;
  programs.fish.enable = true;
  programs.zsh.enable = true;

  home.extraOutputsToInstall = ["/share/zsh"];

  home.sessionVariables = {
    LESSHISTFILE = "$XDG_STATE_HOME/lesshst";
    Z_DATA = "$XDG_DATA_HOME/z";

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

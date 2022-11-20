{
  config,
  lib,
  ...
}: {
  imports = [
    ./bat.nix
    ./home-packages.nix
    ./nixpkgs-config.nix
    ./tealdeer.nix
  ];

  ##: home-manager setup
  programs.home-manager.enable = true;
  manual.json.enable = true;
  news.display = "show";
  xdg.enable = true;

  ##: shells
  programs.bash.enable = true;
  programs.fish.enable = true;
  programs.zsh.enable = true;

  ##: essential tools
  programs.jq.enable = true;
  programs.man.enable = true;
  # N.B. This can slow down builds, but enables more manpage integrations
  # across various tools. See the home-manager manual for more info.
  programs.man.generateCaches = lib.mkDefault true;

  home.sessionVariables = {
    LESSHISTFILE = "$XDG_STATE_HOME/lesshst";
    Z_DATA = "$XDG_DATA_HOME/z";

    # Docker
    # NOTE: disabled so as not to interfere with overriding "docker" with other providers
    # TODO: move this to a docker-specific profile if ever needed.
    # DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
    # MACHINE_STORAGE_PATH = "$XDG_DATA_HOME/docker-machine";

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

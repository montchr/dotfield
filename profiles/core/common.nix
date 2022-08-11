{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (config.lib) dotfield;
in {
  imports = [
    ./cachix.nix
  ];

  nix = {
    package = pkgs.nix;
    useSandbox = lib.mkDefault (!pkgs.stdenv.hostPlatform.isDarwin);
    # FIXME: dangerous
    allowedUsers = ["*"];
    trustedUsers = ["root" "@wheel" "@seadome"];

    gc = {
      automatic = true;
      dates = "weekly";
    };

    extraOptions = ''
      warn-dirty = false
    '';

    # FUP Options {{
    # https://github.com/gytis-ivaskevicius/flake-utils-plus/blob/166d6ebd9f0de03afc98060ac92cba9c71cfe550/lib/options.nix
    linkInputs = true;
    generateRegistryFromInputs = true;
    generateNixPathFromInputs = true;
    # }}
  };

  time.timeZone = "America/New_York";

  environment.variables = {
    DOTFIELD_DIR = dotfield.fsPath;
    EDITOR = "vim";
    KERNEL_NAME =
      if pkgs.stdenv.hostPlatform.isDarwin
      then "darwin"
      else "linux";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
    ZDOTDIR = "$HOME/.config/zsh";

    # Although it points to a commonly-used path for user-owned executables,
    # $XDG_BIN_HOME is a non-standard environment variable. It is not part of
    # the XDG Base Directory Specification.
    # https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
    XDG_BIN_HOME = "$HOME/.local/bin";
  };

  environment.shells = with pkgs; [
    bashInteractive
    fish
    zsh
  ];

  # Install completions for system packages.
  environment.pathsToLink = [
    (lib.optionalString config.programs.fish.enable "/share/fish")
    (lib.optionalString config.programs.zsh.enable "/share/zsh")
  ];

  programs.zsh = {
    enable = lib.mkDefault true;
    enableCompletion = true;
  };

  programs.fish.enable = lib.mkDefault true;

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  environment.systemPackages = with pkgs; [
    # TODO: add this via gitignore.nix or something to avoid IFD
    (writeScriptBin "dotfield"
      (builtins.readFile "${dotfield.srcPath}/bin/dotfield"))

    ## === Essentials ===

    bashInteractive
    bat
    binutils
    cacert
    coreutils
    exa
    fd
    findutils
    gawk
    git
    gnumake
    gnupg
    gnused
    gnutar
    grc
    jq
    less
    moreutils
    openssh
    openssl
    ripgrep
    rsync
    screen
    tmux
    vim

    ## === Network ===

    curl
    dnsutils
    nmap
    wget
    whois

    ## === Files ===

    chafa #    <- "terminal graphics for the 21st century"
    dua #      <- quick disk usage
    file
    glow #     <- a markdown cli renderer (by charmbracelet)
    hexyl #    <- a command-line hex viewer
    mediainfo
    unzip

    # I'm not a fan of broot's keybindings at all... they don't seem to fit any
    # existing paradigm that I've encountered, and many of the defaults seem
    # counterintuitive, undesirable, or inconvenient. Not that I expect
    # everything to "just work", but this feels like a red flag. I'm leaving
    # this here as a note to self because I keep re-installing it...
    # broot #    <- like tree, but in rust

    ## === Nix Helpers ===

    # FIXME: most of these should be removed for servers / non-dev machines

    alejandra # The Uncompromising Nix Code Formatter
    cachix
    fup-repl
    manix # nix documentation search
    nix-diff # Explain why two Nix derivations differ
    nix-tree # Interactively browse dependency graphs of Nix derivations.
    nvfetcher-bin # Generate nix sources expression for the latest version of packages
  ];
}

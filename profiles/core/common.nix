{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (pkgs.lib.our) dotfieldPath;
in {
  imports = [./cachix.nix];

  nix = {
    package = pkgs.nix;
    gc.automatic = true;
    useSandbox = lib.mkDefault (!pkgs.stdenv.hostPlatform.isDarwin);
    allowedUsers = ["*"];
    trustedUsers = ["root" "@wheel" "@seadome"];
    # extraOptions = ''
    #   min-free = 536870912
    #   keep-outputs = true
    #   keep-derivations = true
    #   fallback = true
    # '';

    # FUP Options {{
    # https://github.com/gytis-ivaskevicius/flake-utils-plus/blob/166d6ebd9f0de03afc98060ac92cba9c71cfe550/lib/options.nix
    linkInputs = true;
    generateRegistryFromInputs = true;
    generateNixPathFromInputs = true;
    # }}
  };

  time.timeZone = "America/New_York";

  environment.variables = {
    DOTFIELD_DIR = lib.mkDefault "/etc/dotfield";
    EDITOR = "vim";
    HOSTNAME = config.networking.hostName;
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

  # Enable completions for system packages.
  environment.pathsToLink = ["/share/zsh"];

  programs.zsh = {
    enable = true;
    enableCompletion = true;
  };

  programs.fish.enable = lib.mkDefault false;

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  environment.systemPackages = with pkgs; [
    # TODO: add this via gitignore.nix or something to avoid IFD
    (writeScriptBin "dotfield"
      (builtins.readFile "${dotfieldPath}/bin/dotfield"))

    ## === Essentials ===

    bashInteractive
    bat
    binutils
    cacert
    coreutils
    curl
    dua
    dnsutils
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
    nmap
    openssh
    openssl
    ripgrep
    rsync
    screen
    tmux
    vim
    wget
    whois

    ## === File Helpers ===
    file
    glow # markdown cli renderer (by charmbracelet)
    mediainfo
    unzip

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

{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (pkgs.lib.our) dotfieldPath;
in {
  nix = {
    package = pkgs.nix;
    gc.automatic = true;
    useSandbox = lib.mkDefault true;
    allowedUsers = ["*"];
    trustedUsers = ["root" "@wheel"];
    extraOptions = ''
      min-free = 536870912
      keep-outputs = true
      keep-derivations = true
      fallback = true
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
    DOTFIELD_DIR = lib.mkDefault "/etc/dotfield";

    # If `$DOTFIELD_HOSTNAME` matches `$HOSTNAME`, then we can assume the
    # system has been successfully provisioned with Nix. Otherwise,
    # `$DOTFIELD_HOSTNAME` should remain an empty string.
    DOTFIELD_HOSTNAME = config.networking.hostName;

    CACHEDIR = "$HOME/.cache";
    EDITOR = "vim";
    HOSTNAME = config.networking.hostName;
    KERNEL_NAME =
      if pkgs.stdenv.isDarwin
      then "darwin"
      else "linux";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    TMPDIR = "/tmp";
    XDG_BIN_HOME = "$HOME/.local/bin";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_RUNTIME_DIR = "/tmp";
    XDG_STATE_HOME = "$HOME/.local/state";
    ZDOTDIR = "$HOME/.config/zsh";
  };

  fonts.fonts = [pkgs.emacs-all-the-icons-fonts];

  environment.shells = with pkgs; [
    bashInteractive
    fish
    zsh
  ];

  programs.zsh = {
    enable = true;
    enableCompletion = false;
    enableBashCompletion = false;
    promptInit = "";
  };

  programs.fish.enable = true;

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  environment.systemPackages = with pkgs; [
    # TODO: add this via gitignore.nix or something to avoid IFD
    (writeScriptBin "dotfield"
      (builtins.readFile "${dotfieldPath}/bin/dotfield"))

    (python3.withPackages (ps: with ps; [pip setuptools]))
    (ripgrep.override {withPCRE2 = true;})

    alejandra # The Uncompromising Nix Code Formatter
    bashInteractive
    bat
    binutils
    bottom
    cachix
    coreutils
    curl
    direnv
    dnsutils
    exa
    fd
    findutils
    fup-repl
    fzf
    gawk
    git
    gnumake
    gnupg
    gnused
    gnutar
    grc
    jq
    less
    lua
    manix # nix documentation search
    moreutils
    nix-diff # Explain why two Nix derivations differ
    nix-index
    nix-tree # Interactively browse dependency graphs of Nix derivations.
    nmap

    # Generate nix sources expression for the latest version of packages
    # https://github.com/berberman/nvfetcher
    nvfetcher-bin

    openssh
    openssl

    # A modern replacement for ps written in Rust
    # https://github.com/dalance/procs
    procs

    rsync
    skim
    tealdeer
    tmux
    vim
    wget
    whois
    zsh
  ];
}

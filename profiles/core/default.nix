{ config, lib, pkgs, ... }:

{
  imports = [ ../cachix ];

  nix = {
    # autoOptimiseStore = true;
    gc.automatic = true;
    # optimise.automatic = true;
    useSandbox = true;
    allowedUsers = [ "*" ];
    trustedUsers = [ "root" "@wheel" ];
    extraOptions = ''
      min-free = 536870912
      keep-outputs = true
      keep-derivations = true
      fallback = true
    '';
  };

  services.nix-daemon.enable = true;
  users.nix.configureBuildUsers = true;

  time.timeZone = config.my.timezone;

  environment.variables = {
    # `$DOTFIELD_DIR` must point to its absolute path on the system -- not to
    # its location in the Nix store. Cached shell configuration files may
    # reference a path to an old derivation.
    DOTFIELD_DIR = config.dotfield.path;

    # If `$DOTFIELD_HOSTNAME` matches `$HOSTNAME`, then we can assume the
    # system has been successfully provisioned with Nix. Otherwise,
    # `$DOTFIELD_HOSTNAME` should remain an empty string.
    DOTFIELD_HOSTNAME = config.networking.hostName;

    EDITOR = "vim";
    KERNEL_NAME = if pkgs.stdenv.isDarwin then "darwin" else "linux";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    HOSTNAME = config.networking.hostName;
    TMPDIR = "/tmp";
  };

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

  environment.systemPackages = with pkgs; [
    (writeScriptBin "dotfield"
      (builtins.readFile "${config.dotfield.binDir}/dotfield"))

    bashInteractive
    bat
    bottom
    cachix
    coreutils
    curl
    exa
    fd
    findutils
    fish
    fup-repl
    fzf
    gawk
    gcc
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
    nix-tree # Interactively browse dependency graphs of Nix derivations.
    nvfetcher
    openssl
    (python3.withPackages (ps: with ps; [ pip setuptools ]))
    (ripgrep.override { withPCRE2 = true; })
    rsync
    tmux
    tealdeer
    vim
    wget
    zsh
  ];
}

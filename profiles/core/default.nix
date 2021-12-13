{ config, lib, pkgs, ... }:

{
  imports = [ ../cachix ];

  nix = {
    package = pkgs.nixFlakes;
    gc.automatic = true;
    useSandbox = true;
    allowedUsers = [ "*" ];
    trustedUsers = [ "root" "@wheel" ];
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

    (python3.withPackages (ps: with ps; [ pip setuptools ]))
    (ripgrep.override { withPCRE2 = true; })

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
    nix-index
    nix-tree # Interactively browse dependency graphs of Nix derivations.
    nmap
    nvfetcher
    openssh
    openssl
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

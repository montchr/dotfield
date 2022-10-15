{
  config,
  lib,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
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
  ];
}

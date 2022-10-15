{pkgs, ...}: {
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
    fish
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
    tealdeer
    tmux
    vim

    ## === Network ===

    curl
    dig
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

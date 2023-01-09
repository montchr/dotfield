{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    ## === Essentials ===

    bashInteractive
    bat
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
    (ripgrep.override {withPCRE2 = true;})
    rsync
    screen
    sd
    tealdeer
    tmux
    vim

    (
      if stdenv.hostPlatform.isDarwin
      then clang
      else gcc
    )

    ## === Network ===

    curl
    dig
    dnsutils
    nmap
    wget
    whois

    ## === Monitoring ===

    dua #      <- learn about the disk usage of directories, fast!
    lnav #     <- log file navigator
    procs #    <- a "modern" replacement for ps

    ## === Files ===

    entr #     <- Run arbitrary commands when files change
    du-dust #  <- like du but more intuitive
    file #     <- a program that shows the type of files
    glow #     <- charmbracelet's markdown cli renderer
    hexyl #    <- command-line hex viewer
    unzip #    <- *.zip archive extraction utility
  ];
}

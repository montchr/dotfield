{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    ## === Essentials ===

    bashInteractive
    bat
    cacert
    coreutils
    curl
    dig
    dnsutils
    eza
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
    jql
    less
    moreutils
    nmap
    nushell
    openssh
    openssl
    (ripgrep.override {withPCRE2 = true;})
    rclone
    rsync
    screen
    sd
    tealdeer
    vim
    wget
    whois
    zellij

    (
      if stdenv.hostPlatform.isDarwin
      then clang
      else gcc
    )

    ## === Network ===

    ## === Monitoring ===

    dua #      <- learn about the disk usage of directories, fast!
    lnav #     <- log file navigator
    procs #    <- a "modern" replacement for ps

    ## === Files ===

    du-dust #  <- like du but more intuitive
    file #     <- a program that shows the type of files
    unzip #    <- *.zip archive extraction utility

    ## === Nix Helpers ===

    comma #    <- "run software without installing it"
  ];
}

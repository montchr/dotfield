{ pkgs, ... }:
{
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
    ijq # <- interactive jq :: <https://git.sr.ht/~gpanders/ijq>
    jq
    less
    moreutils
    nh # yet another nix cli helper :: <https://github.com/viperML/nh>
    nmap
    nushell
    openssh
    openssl
    rclone
    (ripgrep.override { withPCRE2 = true; })
    rsync
    screen
    sd
    tealdeer
    ugrep
    vim
    wget
    # FIXME: <https://github.com/NixOS/nixpkgs/issues/260552>
    # whois
    zellij

    (if stdenv.hostPlatform.isDarwin then clang else gcc)

    ## === System Monitoring ===

    dua # <- learn about the disk usage of directories, fast!
    lnav # <- log file navigator
    procs # <- a "modern" replacement for ps

    ## === Files ===

    du-dust # <- like du but more intuitive
    file # <- a program that shows the type of files
    unzip # <- *.zip archive extraction utility

    ## === Nix Helpers ===

    comma # <- "run software without installing it"
  ];
}

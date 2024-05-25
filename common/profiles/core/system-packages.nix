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
    du-dust
    dua
    eza
    fd
    file
    findutils
    fish
    gawk
    gcc
    git
    gnumake
    gnupg
    gnused
    gnutar
    grc
    ijq # <- interactive jq :: <https://git.sr.ht/~gpanders/ijq>
    jq
    less
    lnav
    moreutils
    nh # yet another nix cli helper :: <https://github.com/viperML/nh>
    nmap
    nushell
    openssh
    openssl
    procs
    rclone
    (ripgrep.override { withPCRE2 = true; })
    rsync
    screen
    sd
    tealdeer
    ugrep
    unzip
    vim
    wget
    whois
    yq
    zellij
  ];
}

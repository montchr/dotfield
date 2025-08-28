{ flake, pkgs, ... }:
let
  inherit (flake.perSystem) packages;
in
{
  programs.htop.enable = true;
  programs.mtr.enable = true;

  environment.systemPackages = with pkgs; [
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
    ijq
    inetutils
    iputils
    jq
    less
    lm_sensors
    lnav
    lynx
    moreutils
    nh
    nmap
    oils-for-unix
    openssh
    openssl
    pciutils
    procs
    pv # Tool for monitoring the progress of data through a pipeline
    rclone
    reptyr # reparent a running process to another tty
    (ripgrep.override { withPCRE2 = true; })
    rlwrap # no more "^[[D" in limited shells (a readline wrapper) :: <https://github.com/hanslub42/rlwrap>
    rsync
    screen
    shpool # <- "think tmux, then aim... lower" :: <https://github.com/shell-pool/shpool>
    sysstat
    tealdeer
    unzip
    usbutils
    # TODO: what does this provide?  even after reading
    # <https://git.kernel.org/pub/scm/utils/util-linux/util-linux.git/about/>
    # i still do not know... remove?
    util-linux
    vim
    wget
    whois
    yq
    zellij
  ];
}

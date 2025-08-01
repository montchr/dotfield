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
    dosfstools
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
    # TODO: installer/rescue only?
    gptfdisk
    grc
    gron # <- gr(eppable)(js)on
    ijq
    inetutils
    iputils
    jc # jsonify output of many cli tools
    jq
    jnv # yet another interactive jq thingy
    less
    lm_sensors # <- standard tool for temperature monitoring <https://hwmon.wiki.kernel.org/lm_sensors>
    lnav
    lshw # <- "Provide detailed information on the hardware configuration of the machine" <https://ezix.org/project/wiki/HardwareLiSter>
    lynx
    moreutils
    nh # yet another nix cli helper :: <https://github.com/viperML/nh>
    nmap
    nushell
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
    ugrep
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

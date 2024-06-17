{ lib, pkgs, ... }:
{
  # FIXME: cannot set to false without srvos conflict! even with mkDefault
  # NOTE: Manpage cache generation may add significant time to builds.
  documentation.man.generateCaches = true;

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
    gptfdisk
    grc
    ijq # <- interactive jq :: <https://git.sr.ht/~gpanders/ijq>
    inetutils
    iputils
    jq
    less
    lm_sensors # <- standard tool for temperature monitoring <https://hwmon.wiki.kernel.org/lm_sensors>
    lnav
    lshw # <- "Provide detailed information on the hardware configuration of the machine" <https://ezix.org/project/wiki/HardwareLiSter>
    moreutils
    nh # yet another nix cli helper :: <https://github.com/viperML/nh>
    nmap
    nushell
    openssh
    openssl
    pciutils
    procs
    rclone
    (ripgrep.override { withPCRE2 = true; })
    rsync
    screen
    sd
    sysstat
    tealdeer
    ugrep
    unzip
    usbutils
    util-linux
    vim
    wget
    whois
    yq
    zellij
  ];
}

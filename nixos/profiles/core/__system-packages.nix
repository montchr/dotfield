{ flake, pkgs, ... }:
let
  inherit (flake.perSystem) packages;
in
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
    lynx
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
    # XXX: not in nixpkgs, unable to build, needs release of <https://github.com/shell-pool/shpool/commit/fef785abbf17dc4e4507dea7273cf52f95d92563>
    # packages.shpool # <- "think tmux, then aim... lower" :: <https://github.com/shell-pool/shpool>
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

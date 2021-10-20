{ config, lib, pkgs, ... }:

{
  imports = [ ../cachix ];

  nix = {
    # autoOptimiseStore = true;
    gc.automatic = true;
    # optimise.automatic = true;
    useSandbox = (! pkgs.stdenv.isDarwin);
    allowedUsers = [ "@wheel" ];
    trustedUsers = [ "root" "@wheel" ];
    extraOptions = ''
      min-free = 536870912
      keep-outputs = true
      keep-derivations = true
      fallback = true
    '';
  };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [ ibm-plex inter pragmatapro public-sans ];
  };

  time.timeZone = config.my.timezone;

  environment.variables = { HOSTNAME = config.networking.hostName; };
  environment.shells = with pkgs; [
    bashInteractive
    zsh
  ];

  environment.systemPackages = with pkgs; [
    (writeScriptBin "dotfield"
      (builtins.readFile "${config.dotfield.binDir}/dotfield"))

    bashInteractive
    bat
    bottom
    cachix
    coreutils
    curl
    direnv
    exa
    fd
    findutils
    fzf
    gawk
    gcc
    gnumake
    gnupg
    gnused
    gnutar
    grc
    jq
    less
    lua
    manix # nix documentation search
    nix-tree # Interactively browse dependency graphs of Nix derivations.
    openssl
    ripgrep
    rsync
    tmux
    vim
    wget
    zsh
  ];

  # networking = {
  #  # Use Cloudflare DNS
  #  # https://developers.cloudflare.com/1.1.1.1/
  #  dns = [
  #    "1.1.1.1"
  #    "1.0.0.1"
  #    "2606:4700:4700::1111"
  #    "2606:4700:4700::1001"
  #  ];
  # };
}

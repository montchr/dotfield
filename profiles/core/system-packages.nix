{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.lib) dotfield;

  # TODO: add this via gitignore.nix or something to avoid IFD
  dotfieldScript =
    pkgs.writeScriptBin "dotfield"
    (builtins.readFile "${dotfield.srcPath}/bin/dotfield");
in {
  environment.systemPackages = with pkgs; [
    dotfieldScript

    ## === Essentials ===

    bashInteractive
    bat
    binutils
    cacert
    coreutils
    exa
    fd
    findutils
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
    tmux
    vim

    ## === Network ===

    curl
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

    # I'm not a fan of broot's keybindings at all... they don't seem to fit any
    # existing paradigm that I've encountered, and many of the defaults seem
    # counterintuitive, undesirable, or inconvenient. Not that I expect
    # everything to "just work", but this feels like a red flag. I'm leaving
    # this here as a note to self because I keep re-installing it...
    # broot #    <- like tree, but in rust

    ## === Nix Helpers ===

    # FIXME: most of these should be removed for servers / non-dev machines

    alejandra # The Uncompromising Nix Code Formatter
    cachix
    fup-repl
    manix # nix documentation search
    nix-diff # Explain why two Nix derivations differ
    nix-tree # Interactively browse dependency graphs of Nix derivations.
    nvfetcher-bin # Generate nix sources expression for the latest version of packages
  ];
}

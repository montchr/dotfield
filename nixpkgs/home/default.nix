{ config, pkgs, lib, ... }:
let
  tmp_directory = "/tmp";
  home_directory = "${config.home.homeDirectory}";
in
rec {
  imports = [
    ./git.nix
    ./kitty.nix
    # TODO: check it out?
    # ./newsboat.nix
    ./shells.nix
  ];

  home = {
    stateVersion = "21.05";

    sessionVariables = {
      # TODO: use emacs
      # EDITOR = "${pkgs.vim}/bin/vim";
      EMAIL = "${config.programs.git.userEmail}";
      # TODO: there is another option... but what is it?
      PAGER = "${pkgs.less}/bin/less";
    };

    packages = with pkgs; [
      asciinema
      cacert
      cachix
      coreutils
      curl
      exa
      fd
      findutils
      getopt
      gnumake
      gnupg
      gpgme
      htop
      jq
      less
      nodePackages.node2nix
      # nodePackages.vim-language-server
      pass
      # plantuml
      pywal
      # TODO: is this the perl rename, or the less-useful one?
      # renameutils
      ripgrep
      rsync
      shellcheck
      shfmt
      # TODO: what is this? i keep seeing it
      # universal-ctags
      # TODO: investigate
      # urlscan
      vim
      vim-vint
      wget
    ];
  };

  programs = {

    home-manager = { enable = true; };

    # TODO: ?
    # browserpass = {
    #   enable = true;
    #   browsers = [ "firefox" ];
    # };

    direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
    };

    # TODO: works on macOS?
    dircolors = {
      enable = true;
      enableZshIntegration = true;
    };

    fzf = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
    };

    ssh = {
      enable = true;

      # TODO: ?
      controlMaster = "auto";
      controlPath = "${tmp_directory}/ssh-%u-%r@%h:%p";
      controlPersist = "1800";

      forwardAgent = true;
      serverAliveInterval = 60;

      hashKnownHosts = true;
    };
  };

  xdg = {
    enable = true;

    configHome = "${home_directory}/.config";
    dataHome = "${home_directory}/.local/share";
    cacheHome = "${home_directory}/.cache";
  };
}

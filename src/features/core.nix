{
  self,
  lib,
  ...
}:
let

  # Although a convention for storing personal executable scripts and
  # binaries outside of system-wide paths, $XDG_BIN_HOME is not part of
  # the XDG Base Directory Specification.
  #
  # https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
  #
  # NOTE: This may also be set at the system level -- it is included again here
  # for standalone installation parity.
  binHome = "$HOME/.local/bin";
in
{
  aspects.core = {
    nixos =
      { config, pkgs, ... }:
      {
        services.dbus.implementation = "broker";
        hardware.enableRedistributableFirmware = true;

        environment.variables = {
          EDITOR = "vim";
          HOSTNAME = config.networking.hostName;
          LANG = "en_US.UTF-8";
          LC_ALL = "en_US.UTF-8";
          XDG_CACHE_HOME = "$HOME/.cache";
          XDG_CONFIG_HOME = "$HOME/.config";
          XDG_DATA_HOME = "$HOME/.local/share";
          XDG_STATE_HOME = "$HOME/.local/state";
          XDG_BIN_HOME = binHome;
        };

        # Select internationalisation properties.
        i18n.defaultLocale = "en_US.UTF-8";
        i18n.extraLocaleSettings = {
          LC_ADDRESS = "en_US.UTF-8";
          LC_IDENTIFICATION = "en_US.UTF-8";
          LC_MEASUREMENT = "en_US.UTF-8";
          LC_MONETARY = "en_US.UTF-8";
          LC_NAME = "en_US.UTF-8";
          LC_NUMERIC = "en_US.UTF-8";
          LC_PAPER = "en_US.UTF-8";
          LC_TELEPHONE = "en_US.UTF-8";
          LC_TIME = "en_US.UTF-8";
        };

        programs.htop.enable = true;
        programs.mtr.enable = true;

        environment.systemPackages = with pkgs; [
          bashInteractive
          bat # :: better cat
          bc # :: GNU software calculator
          btop
          cacert
          coreutils
          curl
          dig
          diskus # :: simple `du -sh` alternative by sharkdp
          dnsutils
          dua
          dust
          exfatprogs
          eza
          fd
          file
          findutils
          fish
          fswatch
          gawk
          gcc
          git
          gnumake
          gnupg
          gnused
          gnutar
          grc
          hl-log-viewer
          htop
          hyperfine
          ijq # interactive jq
          inetutils
          iputils
          jq
          less
          lm_sensors
          lnav # log file pager
          lsof
          lynx
          moreutils
          nh
          nmap
          oils-for-unix
          openssh
          openssl
          pciutils
          procs
          psmisc # utils for working with proc fs (e.g. fuser, killall, pstree)
          pv # monitor the progress of data through a pipeline
          rclone # rsync for "the cloud"
          reptyr # reparent a running process to another tty
          (ripgrep.override { withPCRE2 = true; })
          rlwrap # no more "^[[D" in limited shells (a readline wrapper) :: <https://github.com/hanslub42/rlwrap>
          rsync # thee file transfer tool
          screen
          shpool # <- "think tmux, then aim... lower" :: <https://github.com/shell-pool/shpool>
          sysstat
          tealdeer # provides "tldr"
          unzip
          usbutils
          util-linux
          vim
          watchexec
          wget # baby curls
          whois
          yq
          zellij
        ];
      };

    home =
      { pkgs, config, ... }:
      {
        programs.home-manager.enable = true;
        manual.json.enable = true;
        news.display = "show";
        xdg.enable = true;

        programs.bash.enable = true;
        programs.jq.enable = true;
        programs.man.enable = true;

        # User-defined executables should always be prioritized in $PATH.
        home.sessionPath = lib.mkBefore [ binHome ];

        home.sessionVariables = {
          "EDITOR" = lib.mkDefault "vim";
          "LESSHISTFILE" = "${config.xdg.stateHome}/lesshst";
        };

        home.packages = [
          pkgs.bttf # BurntSushi's datetime calc/parser/formatting utility
          pkgs.csvkit # Standard toolkit for CSV manipulation
        ];
      };
  };
}

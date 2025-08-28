{
  self,
  ops,
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
      };

    home =
      { config, ... }:
      {
        _module.args = {
          inherit ops;
        };

        ### home-manager setup
        programs.home-manager.enable = true;
        manual.json.enable = true;
        news.display = "show";
        xdg.enable = true;

        ### shells
        programs.bash.enable = true;

        ### essential tools
        programs.jq.enable = true;
        programs.man.enable = true;

        # User-defined executables should always be prioritized in $PATH.
        home.sessionPath = lib.mkBefore [ binHome ];

        home.sessionVariables = {
          "EDITOR" = lib.mkDefault "vim";
          "LESSHISTFILE" = "${config.xdg.stateHome}/lesshst";
          "XDG_BIN_HOME" = binHome;
        };
      };
  };
}

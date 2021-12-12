{ config, hmUsers, pkgs, ... }:
let
  inherit (hmUsers) cdom;
  inherit (user) githubUsername username keys xdg;

  user = hmUsers.cdom;
in
{
  home-manager.users = { inherit cdom; };

  users.users.${username} = {
    inherit (user) home;

    isNormalUser = true;
    uid = 1000;
    # FIXME: this is no password!
    password = username;
    extraGroups = [ "wheel" "networkmanager" ];
    openssh.authorizedKeys.keys = [ keys.primary.ssh ] ++ keys.additional.ssh;
    packages = with pkgs; [
      asciinema
      # bandwhich # display current network utilization by process
      bottom # fancy version of `top` with ASCII graphs
      cacert
      cachix
      circleci-cli
      du-dust
      exa
      fd
      findutils
      gawk
      getopt
      gnupg
      gnused
      gnutar
      grex # Generate regexps from user-provided test cases
      hyperfine
      lua
      jq
      less
      lnav # System Log file navigator
      pandoc
      pass
      ripgrep
      rsync
      shellcheck
      shfmt
      tmux
      universal-ctags
      vim
      vim-vint
      wget
      yamllint
      yq
      zoxide
      zsh
    ];
  };

  #user.activationScripts = {
  #  gnupgHomePermissions = home.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  #   sudo chown -R ${config.home.username} ${gnupgHome}
  #  find ${gnupgHome} -type f -exec sudo chmod 600 {} \;
  # find ${gnupgHome} -type d -exec sudo chmod 700 {} \;
  #  '';
  #};

  environment = {
    variables = {
      # `$DOTFIELD_DIR` must point to its absolute path on the system -- not
      # to its location in the Nix store. ZSH may cache a path to an old
      # derivation.
      DOTFIELD_DIR = config.dotfield.path;

      # If `$DOTFIELD_HOSTNAME` matches `$HOSTNAME`, then we can assume the
      # system has been successfully provisioned with Nix. Otherwise,
      # `$DOTFIELD_HOSTNAME` should remain an empty string.
      DOTFIELD_HOSTNAME = config.networking.hostName;

      DOTFIELD_PGP_KEY = user.keys.primary.pgp;

      XDG_BIN_HOME = "${xdg.bin}";
      XDG_CACHE_HOME = "${xdg.cache}";
      XDG_CONFIG_HOME = "${xdg.config}";
      XDG_DATA_HOME = "${xdg.data}";

      GITHUB_USER = githubUsername;
      LESSHISTFILE = "${xdg.cache}/lesshst";
      WGETRC = "${xdg.config}/wgetrc";
    };
  };
}

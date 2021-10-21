{ options, config, lib, pkgs, ... }:
let
  inherit (config) my;
in
{
  users.users.${my.username} = lib.mkAliasDefinitions options.my.user;

  my.user.home =
    if pkgs.stdenv.isDarwin then
      "/Users/${my.username}"
    else
      "/home/${my.username}";

  my.env = {
    # Default is "1". But when typeset in PragmataPro that leaves no space
    # between the icon and its filename.
    EXA_ICON_SPACING = "2";

    GITHUB_USER = my.githubUsername;
    LESSHISTFILE = "${my.xdg.data}/lesshst";
    WGETRC = "${my.xdg.config}/wgetrc";

    # zoxide
    Z_DATA = "$XDG_DATA_HOME/z";
  };

  my.user.packages = import ./package-list.nix { inherit pkgs; };

  environment =
    let
      extraVars = lib.concatStringsSep "\n"
        (lib.mapAttrsToList (n: v: ''export ${n}="${v}"'') my.env);
    in
    {
      extraInit = ''
        # Check whether a command exists.
        has() {
          type "$1" >/dev/null 2>&1
        }

        ${extraVars}

        ${lib.strings.fileContents ./appearance.sh}
      '';

      variables = with my; {
        CACHEDIR = xdg.cache;
        XDG_BIN_HOME = xdg.bin;
        XDG_CACHE_HOME = xdg.cache;
        XDG_CONFIG_HOME = xdg.config;
        XDG_DATA_HOME = xdg.data;
        XDG_RUNTIME_DIR = "/tmp";

        PATH = [ xdg.bin "$PATH" ];
        INPUTRC = "${xdg.config}/readline/inputrc";

        # Appearance
        BASE16_THEME_DARK = "black-metal-khold";
        BASE16_THEME_LIGHT = "grayscale-light";
        CDOM_EMACS_THEME_DARK = "modus-vivendi";
        CDOM_EMACS_THEME_LIGHT = "modus-operandi";

        # Docker
        DOCKER_CONFIG = "${xdg.config}/docker";
        MACHINE_STORAGE_PATH = "${xdg.data}/docker-machine";

        # Go
        GOPATH = "${xdg.data}/go";

        # Ruby
        BUNDLE_USER_CACHE = "${xdg.cache}/bundle";
        BUNDLE_USER_CONFIG = "${xdg.config}/bundle";
        BUNDLE_USER_PLUGIN = "${xdg.data}/bundle";

        # Rust
        CARGO_HOME = "${xdg.data}/cargo";
        RUSTUP_HOME = "${xdg.data}/rustup";

        # GNU screen
        SCREENRC = "${xdg.config}/screen/screenrc";

        # Vagrant
        VAGRANT_ALIAS_FILE = "${xdg.data}/vagrant/aliases";
        VAGRANT_HOME = "${xdg.data}/vagrant";

        # wd
        # https://github.com/mfaerevaag/wd
        WD_CONFIG = "${xdg.config}/wd/warprc";

        Z_OWNER = my.username;
      };

    };

  my.hm.home = {
    # Necessary for home-manager to work with flakes, otherwise it will
    # look for a nixpkgs channel.
    stateVersion =
      if pkgs.stdenv.isDarwin then
        "21.11"
      else
        config.system.stateVersion;
    username = my.username;
  };

  my.hm.xdg.enable = true;

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${my.username} = lib.mkAliasDefinitions options.my.hm;
  };
}

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
    _ZO_DATA_DIR = "$XDG_DATA_HOME/zoxide";
  };

  my.user.packages = import ./package-list.nix { inherit pkgs; };

  environment = {
    variables = with my; {
      # `$DOTFIELD_DIR` must point to its absolute path on the system -- not
      # to its location in the Nix store. ZSH may cache a path to an old
      # derivation.
      DOTFIELD_DIR = config.dotfield.path;

      # If `$DOTFIELD_HOSTNAME` matches `$HOSTNAME`, then we can assume the
      # system has been successfully provisioned with Nix. Otherwise,
      # `$DOTFIELD_HOSTNAME` should remain an empty string.
      DOTFIELD_HOSTNAME = config.networking.hostName;

      XDG_BIN_HOME = "${xdg.bin}";
      XDG_CACHE_HOME = "${xdg.cache}";
      XDG_CONFIG_HOME = "${xdg.config}";
      XDG_DATA_HOME = "${xdg.data}";
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

  environment.extraInit = lib.concatStringsSep "\n"
    (lib.mapAttrsToList (n: v: ''export ${n}="${v}"'') my.env);
}
